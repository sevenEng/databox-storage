open Lwt
open Opium.Std
open Store_misc


module S = Json_store.JSON_Store

let store_handle = ref None

let store_init ?log_root () =
  let config = Irmin_unix.Irmin_git.config () in
  let log_key = "better get a random string?" |> Cstruct.of_string in
  S.create ~log_key ?log_root config >>= fun s ->
  store_handle := Some s;
  return_unit

let rec store () = match !store_handle with
  | None -> store_init () >>= store
  | Some s -> return s



let read =
  get "/api/key/:key" begin fun req ->
    let key = param req "key" |> extract_key in
    store () >>= fun s ->
    let f () =
      S.read s key >>= fun j ->
      respond' ~headers (`Json j)
    in
    let exn = function
      | Not_found -> not_found_response key
      | _ as err -> err_response err in
    catch f exn
  end


let update =
  post "/api/key/:key" begin fun req ->
    let key = param req "key" |> extract_key in
    let v = Request.body req in
    store () >>= fun s ->
    Cohttp_lwt_body.to_string v >>= fun v ->
    let f () =
      let j = Ezjsonm.from_string v in
      S.update s key j >>= fun () ->
      respond' ~headers (`String "")
    in
    catch f err_response
  end


let list =
  get "/api/list/key/:key" begin fun req ->
    let key = param req "key" |> extract_key in
    store () >>= fun s ->
    let f () =
      S.list s key >>= fun sub_keys ->
      let sub_keys = List.map (fun k -> String.concat "/" k) sub_keys in
      let j = Ezjsonm.strings sub_keys in
      `Json j |> respond' ~headers
    in
    catch f err_response
  end


let remove =
  post "/api/remove/:key" begin fun req ->
    let key = param req "key" |> extract_key in
    store () >>= fun s ->
    let f () =
      S.remove s key >>= fun () ->
      respond' ~headers (`String "")
    in
    catch f err_response
  end


let rremove =
  post "/api/rremove/:key" begin fun req ->
    let key = param req "key" |> extract_key in
    store () >>= fun s ->
    let f () =
      S.remove_rec s key >>= fun () ->
      respond' ~headers (`String "")
    in
    catch f err_response
  end


let logs =
  get "/api/logs/:max" begin fun req ->
    let max = param req "max" in
    store () >>= fun s ->
    let f () =
      let max = int_of_string max in
      S.logs s ~max () >>= fun logs ->
      let j = Ezjsonm.strings logs in
      `Json j |> respond' ~headers
    in
    catch f err_response
  end


let is_valide =
  get "/api/logs/validate" begin fun req ->
    let uri = Request.uri req in
    let all =
      Uri.query uri
      |> List.assoc "all"
      |> fun l -> try List.hd l |> bool_of_string
                  with _ -> false
    in
    store () >>= fun s ->
    let f () =
      S.is_valide s ~all () >>= fun validity ->
      let j = Ezjsonm.(
        ["valide", validity |> bool;
         "all", all |> bool]
        |> dict)
      in
      `Json j |> respond' ~headers
    in
    catch f err_response
  end


let json_store_app =
  app_with_uri_converter
  |> read
  |> update
  |> list
  |> remove
  |> rremove
  |> logs
  |> is_valide


let () =
  let log_root =
    try Some (Sys.getenv "LOGROOT")
    with Not_found -> None
  in
  Lwt_main.run (store_init ?log_root ());
  App.run_command json_store_app