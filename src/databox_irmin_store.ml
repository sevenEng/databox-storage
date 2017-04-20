open Lwt.Infix

module C  = Cohttp
module Ez = Ezjsonm
module R  = Rresult.R

module Server = Cohttp_lwt_unix.Server


let make_err_response ~status ?error () =
  let body =
    let obj = `O [
        "status", `String (C.Code.string_of_status status);
        "error", `String (match error with None -> "" | Some e -> e)]
    in
    Cohttp_lwt_body.of_string (Ez.to_string obj)
  in
  let resp = C.Response.make ~status () in
  resp, body


let callback (kv, ts) _ req body =
  Store_macaroons.macaroon_request_checker req >>= fun mvalid ->
  if R.is_ok mvalid then
    let uri =
      C.Request.uri req
      |> Uri.to_string
      |> Astring.String.cuts ~empty:false ~sep:"/"
    in
    let meth = C.Request.meth req in
    Cohttp_lwt_body.to_string body >>= fun body ->
    match uri with
    | ["cat"] -> Store_hypercat.handler meth body
    | [key; "kv"] -> Store_kv.handler kv meth ~key ~body
    | sourceid :: "ts" :: action ->
        let action =
          match action with
          | [] -> None | a :: _ -> Some a
        in
        Store_ts.handler ts meth ~sourceid ~body ?action ()
    | _ ->
        let status = `Not_implemented in
        let error = C.Code.string_of_method meth in
        Lwt.return @@ make_err_response ~status ~error ()
  else begin
    let status = `Unauthorized in
    let error =
      let err = R.get_error mvalid in
      Format.(fprintf str_formatter "Missing/Invalid key/token: %a" R.pp_msg err;
      flush_str_formatter ())
     in
    Lwt.return @@ make_err_response ~status ~error ()
  end


let gen_mode ?port () =
  let p =
    match port with
    | None -> Store_env.local_port () |> int_of_string
    | Some p -> p
  in

  let certs = Store_env.https_certs () in
  if R.is_ok certs then
    let cpath, kpath = R.get_ok certs in
    `TLS (`Crt_file_path (Fpath.to_string cpath),
          `Key_file_path (Fpath.to_string kpath),
          `No_password, `Port p)
  else `TCP (`Port p)


let main ?secret ?log_root ?port () =
  Store_macaroons.init ?secret () >>= fun () ->

  let config = Irmin_unix.Irmin_git.config () in
  let kv_key = "log key for kv store" |> Cstruct.of_string
  and ts_key = "log key for ts store" |> Cstruct.of_string in
  Store_engine.JSON_Store.create ~log_key:kv_key ?log_root config >>= fun kv ->
  Store_engine.JSON_Store.create ~log_key:ts_key ?log_root config >>= fun ts ->

  let mode = gen_mode ?port () in
  let s = Server.make (callback (kv, ts)) () in
  Server.create ~mode s
