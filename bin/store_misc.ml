open Opium.Std

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

let app_with_uri_converter = App.empty |> middleware uri_conv_mw
