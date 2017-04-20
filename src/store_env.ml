let local_name = ref "databox-irmin-store"
let local_port = ref "8080"

let arbiter_endpoint = ref "https://databox-arbiter:8080"
let arbiter_key = ref ""

let local_name () =
  try Sys.getenv "DATABOX_LOCAL_NAME"
  with Not_found -> !local_name


let local_port () =
  try Sys.getenv "DATABOX_LOCAL_PORT"
  with Not_found -> !local_port


let arbiter_endp () =
  try Sys.getenv "DATABOX_ARBITER_ENDPOINT"
  with Not_found -> !arbiter_endpoint


let arbiter_token () =
  try Sys.getenv "ARBITER_TOKEN"
  with Not_found -> !arbiter_key


let https_certs () =
  let open Rresult.R in
  let env_cert =
    try Sys.getenv "HTTPS_SERVER_CERT"
    with Not_found -> ""
  and env_key  =
    try Sys.getenv "HTTPS_SERVER_PRIVATE_KEY"
    with Not_found -> ""
  in

  if env_cert = "" || env_key = "" then error_msg "no cert or key" else
  let open Bos.OS in
  Dir.user () >>= fun user ->
  let cert_dir =
    Fpath.add_seg user "certs"
    |> Fpath.to_dir_path
  in
  Dir.create cert_dir >>= fun _ ->

  let file_cert = Fpath.add_seg cert_dir "public.cert"
  and file_key  = Fpath.add_seg cert_dir "private.key" in
  File.delete file_cert >>= fun () ->
  File.delete file_key  >>= fun () ->
  File.write file_cert env_cert >>= fun () ->
  File.write file_key env_key   >>= fun () ->
  ok (file_cert, file_key)
