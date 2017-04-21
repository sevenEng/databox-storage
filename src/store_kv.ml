open Lwt.Infix

module S  = Store_engine.JSON_Store
module Ez = Ezjsonm
module C  = Cohttp


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


let sub s ~key fn =
  let diff_process = function
    | `Added v | `Updated (_, v) -> Lwt.return @@ fn (Ez.to_string v)
    | `Removed _ -> Lwt.return_unit
  in
  S.watch_key s [key] diff_process


let handler s meth ~key ~body =
  match meth with
  | `GET ->
      Lwt.catch (fun () ->
          S.read s [key] >>= fun v ->
          let b = Ez.to_string v in
          let body = Cohttp_lwt_body.of_string b in
          let resp = C.Response.make ~status:`OK () in
          Lwt.return (resp, body)) (fun _ ->
          let status = `Not_found in
          let error = "Document not found" in
          Lwt.return @@ make_err_response ~status ~error ())

  | `POST ->
      Lwt.catch (fun () ->
          let v = Ez.from_string body in
          S.update s [key] v >>= fun () ->
          let resp = C.Response.make ~status:`OK () in
          Lwt.return (resp, Cohttp_lwt_body.empty)) (fun exn ->
          let status = `Internal_server_error in
          let error = Printexc.to_string exn in
          Lwt.return @@ make_err_response ~status ~error ())
  | _ ->
      let status = `Not_implemented in
      let error = C.Code.string_of_method meth in
      Lwt.return @@ make_err_response ~status ~error ()
