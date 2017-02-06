open Lwt
open Opium.Std
open Json_store



module S = JSON_Store

let store_handle = ref None

let store_init () =
  let config = Irmin_unix.Irmin_git.config () in
  let key = "better get a random string?" |> Cstruct.of_string in
  S.create ~key config >>= fun s ->
  store_handle := Some s;
  return_unit

let rec store () = match !store_handle with
  | None -> store_init () >>= store
  | Some s -> return s



let headers =
  let headers = ["Content-Type"," application/json"] in
  Cohttp.Header.of_list headers

let not_found_response k =
  let open Ezjsonm in
  let obj =
    ["status", `Not_found |> Cohttp.Code.string_of_status |> string;
     "error", ("not found:" ^ (String.concat "/" k)) |> string]
  in
  let body = obj |> dict in
  let code = `Not_found in
  `Json body |> respond' ~headers ~code

let err_response err =
  let msg = Printexc.to_string err in
  let open Ezjsonm in
  let obj =
    ["status", `Internal_server_error |> Cohttp.Code.string_of_status |> string;
     "error", msg |> string]
  in
  let body = obj |> dict in
  let code = `Internal_server_error in
  `Json body |> respond' ~headers ~code



let extract_key conv_key =
  let open Astring in
  String.cuts ~sep:"-" ~empty:false conv_key


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



let uri_conv_mw =
  let filter handler req =
    let concat_key uri =
      let path = uri |> Uri.path |> Uri.pct_decode in
      let open Astring in
      let steps = String.cuts ~empty:false ~sep:"/" path in
      let path' =
        let rec aux acc = function
          | hd :: tl when hd = "key" ->
             let key = String.concat ~sep:"-" tl in
             key :: hd :: acc
          | hd :: tl -> aux (hd :: acc) tl
          | _ -> acc in
        aux [] steps
        |> List.rev
        |> String.concat ~sep:"/"
      in
      Uri.with_path uri path'
    in
    let uri' =
      req
      |> Request.uri
      |> concat_key
    in
    let req' =
      let co_req = Request.request req in
      let meth = Cohttp.Request.meth co_req in
      let version = Cohttp.Request.version co_req in
      let encoding = Cohttp.Request.encoding co_req in
      let co_req' = Cohttp.Request.make ~meth ~version ~encoding uri' in
      {req with request = co_req'}
    in
    handler req'
  in
  Opium_rock.Middleware.create ~filter ~name:"uri converter"



let opium_app =
  App.empty
  |> read
  |> update
  |> list
  |> remove
  |> rremove
  |> middleware uri_conv_mw


let () =
  Lwt_main.run (store_init ());
  App.run_command opium_app
