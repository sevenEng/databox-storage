open Lwt

module R  = Rresult.R
module Fr = Websocket_cohttp_lwt.Frame
module C  = Cohttp
module Ez = Ezjsonm

type clientid = string
type frame     = Fr.t
type subject   = [`Key of string | `Sourceid of string]
type stopper   = unit -> unit Lwt.t


let fn_tbl : (clientid, frame option -> unit) Hashtbl.t  = Hashtbl.create 13

let sub_tbl : (clientid, (subject * stopper) list) Hashtbl.t = Hashtbl.create 13


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


let sub (kv, _) id ~subject =
  let subs = try Hashtbl.find sub_tbl id with _ -> [] in
  if List.mem_assoc subject subs then return_unit
  else begin
    let fn = fun str ->
      let f = Hashtbl.find fn_tbl id in
      f @@ Some (Fr.create ~content:str ())
    in
    (match subject with
    | `Key key -> Store_kv.sub kv ~key fn
    | `Sourceid sourceid -> Store_ts.sub ~sourceid ~clientid:id fn)
    >>= fun stopper ->
    Hashtbl.replace sub_tbl id ((subject, stopper) :: subs);
    return_unit
  end


let unsub id ~subject =
  let subs = try Hashtbl.find sub_tbl id with _ -> [] in
  if not @@ List.mem_assoc subject subs then return_unit
  else begin
    let stopper = List.assoc subject subs in
    stopper () >>= fun () ->
    Hashtbl.replace sub_tbl id (List.filter (fun (s, _) -> s <> subject) subs);
    return_unit
  end


let client_fr_recv id fr =
  let open Fr in
  let () = Logs.debug (fun m -> m "[databox-irmin] client fr recv: %a" Fr.pp fr) in
  let push_fr = Hashtbl.find fn_tbl id in
  match fr.opcode with
  | Opcode.Ping -> push_fr @@ Some (create ~opcode:Opcode.Pong ())
  | Opcode.Close ->
      if String.length fr.content >= 2 then
         let content = Astring.(String.(sub ~start:0 ~stop:2 fr.content |> Sub.to_string)) in
         push_fr @@ Some (create ~opcode:Opcode.Close ~content ())
      else push_fr @@ Some (close 1000);

      let stps = try Hashtbl.find sub_tbl id with _ -> [] in
      Lwt.async (fun () -> Lwt_list.iter_p (fun (_, stp) -> stp ()) stps);
      Hashtbl.remove sub_tbl id;
      Hashtbl.remove fn_tbl id
  | _ -> ()


let register flow req =
  let clientid = Store_macaroons.get_client_id req in
  (if R.is_ok clientid then
     let id = R.get_ok clientid in

     if Hashtbl.mem fn_tbl id then Lwt.return R.(error @@ msg ("connection existed for " ^ id))
     else begin
       Websocket_cohttp_lwt.upgrade_connection req flow (client_fr_recv id)
       >>= fun (resp, body, push_fr) ->
       Hashtbl.replace fn_tbl id push_fr;
       Lwt.return @@ R.ok (resp, body)
     end
   else Lwt.return @@ R.(error (msg "can't parse identity from macaroon")))
  >>= function
  | Ok (resp, body) -> Lwt.return (resp, body)
  | Error err ->
      let status = `Unauthorized in
      let error =
        Format.(fprintf str_formatter "open websocket connection: %a" R.pp_msg err;
                flush_str_formatter ())
      in
      Lwt.return @@ make_err_response ~status ~error ()


let handler s ~clientid ~subject ~action =
  if not @@ Hashtbl.mem fn_tbl clientid then
    let status = `Conflict in
    let error = "No open WebSocket connection to \
                 client; WebSocket connection must \
                 exist before subscription requests" in
    Lwt.return @@ make_err_response ~status ~error ()
  else begin
    match action with
    | "sub" ->
        sub s clientid ~subject
        >>= fun () ->
        let resp = C.Response.make ~status:`OK () in
        Lwt.return @@ (resp, Cohttp_lwt_body.empty)
    | "unsub" ->
        unsub clientid ~subject
        >>= fun () ->
        let resp = C.Response.make ~status:`OK () in
        Lwt.return @@ (resp, Cohttp_lwt_body.empty)
    | _ ->
        let status = `Not_found in
        let error = "Not supported action: " ^ action in
        Lwt.return @@ make_err_response ~status ~error ()
  end





