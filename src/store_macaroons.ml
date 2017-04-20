open Lwt

module Client   = Cohttp_lwt_unix.Client
module Macaroon = Sodium_macaroons
module R        = Rresult.R

let s = ref None

let get_secret () =
  let endp = Uri.of_string (Store_env.arbiter_endp ()) in
  let url = Uri.with_path endp "/store/secret" in

  let h = ["X-Api-Key", Store_env.arbiter_token ()] in
  let headers = Cohttp.Header.of_list h in

  Client.get ~headers url >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  let code = Cohttp.Code.code_of_status status in

  if not @@ Cohttp.Code.is_success code then
    Cohttp_lwt_body.to_string body >>= fun err_msg ->
    Logs_lwt.err (fun m -> m "[macaroon] no secret from arbiter: %s" err_msg)
    >>= fun () ->
    return_unit
  else
    Cohttp_lwt_body.to_string body >>= fun body ->
    s := Some (B64.decode body);
    return_unit


let init ?secret () =
  match secret with
  | None -> get_secret ()
  | Some s' -> s := Some s'; return_unit


let secret () =
  let repeat = 3 in
  let rec aux cnt =
    match !s with
    | None ->
        if cnt >= repeat then return @@ R.error_msg "Can't get macaroon secret"
        else Logs_lwt.debug (fun m -> m
          "[macaroon] try to get macaroon secret %d/%d" cnt repeat)
          >>= get_secret >>= fun () -> aux @@ succ cnt
    | Some s -> return @@ R.ok s
  in
  aux 1


let verify_target caveat_str =
  let expected = "target = " ^ Store_env.local_name () in
  expected = caveat_str


let verify_method  meth caveat_str =
  let meth =
    meth
    |> Cohttp.Code.string_of_method
    |> String.lowercase_ascii
  in
  let expected = "method = " ^ meth in
  let caveat_str' = String.lowercase_ascii caveat_str in
  expected = caveat_str'


let verify_path url caveat_str =
  let path = Uri.path url in
  let expected = "path = " ^ path in
  expected = caveat_str


let verify macaroon key uri meth =
  let open R in
  if not @@ is_ok macaroon then error @@ get_error macaroon
  else if not @@ is_ok key then error @@ get_error key
  else begin
    let macaroon = get_ok macaroon
    and key      = get_ok key in

    let check str =
      Logs.debug (fun m -> m "[macaroon] check for %s..." str);
      let f verifier = verifier str in
      let l = [
        verify_target;
        verify_method meth;
        verify_path uri;] in
      List.exists f l
    in

    return @@ Macaroon.verify macaroon ~key ~check []
  end


let extract_macaroon headers =
  let open R in
  (match Cohttp.Header.get headers "x-api-key" with
  | Some m -> ok m
  | None -> begin
      match Cohttp.Header.get_authorization headers with
      | Some (`Basic (name, _)) -> ok name
      | _ -> error_msg "Missing API key/token" end)
  >>= fun t ->
  try
    match Macaroon.deserialize t with
    | `Ok m -> ok m
    | `Error (_, _) -> error_msg "Invalid API key/token"
  with Not_found -> error_msg "deserialization problem"


let macaroon_request_checker request =
  let uri = Cohttp.Request.uri request
  and meth = Cohttp.Request.meth request
  and headers = Cohttp.Request.headers request in

  let macaroon = extract_macaroon headers in
  if R.is_error macaroon then return R.(error (get_error macaroon))
  else begin
    secret () >>= fun key ->
    if R.is_error key then return @@ R.(error (get_error key))
    else begin
      let r = verify macaroon key uri meth in
      return @@ R.ok ()
    end
  end
