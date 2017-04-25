open Lwt.Infix

module S  = Store_engine.JSON_Store
module Ez = Ezjsonm
module C  = Cohttp


let now () =
  Ptime_clock.now ()
  |> Ptime.to_float_s
  |> (fun s -> s *. 1000.)
  |> int_of_float
  |> string_of_int


let obj_of_body b = Ez.(
    from_string b
    |> value
    |> get_dict)


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


type clientid = string
type sourceid  = string
let watchers_tbl : (sourceid, (clientid * (string -> unit)) list) Hashtbl.t = Hashtbl.create 13


let stopper sourceid id () =
  let fs = try Hashtbl.find watchers_tbl sourceid with _ -> [] in
  if not @@ List.mem_assoc id fs then Lwt.return_unit
  else begin
    let fs' = List.filter (fun (cid, _) -> cid <> id) fs in
    Hashtbl.replace watchers_tbl sourceid fs';
    Lwt.return_unit
  end


let sub ~sourceid ~clientid fn =
  let fs = try Hashtbl.find watchers_tbl sourceid with _ -> [] in
  if List.mem_assoc clientid fs then () else
  Hashtbl.replace watchers_tbl sourceid ((clientid, fn) :: fs);
  Lwt.return @@ stopper sourceid clientid


let sourceid_updated ~sourceid data =
  let fs = try Hashtbl.find watchers_tbl sourceid with _ -> [] in
  let str = Ez.(data |> wrap |> to_string) in
  Lwt.return @@ List.map (fun (_, f) -> f str) fs


let handler s meth ~sourceid ?action ~body () =
  match meth with
  | `POST when action = None ->
      let data = obj_of_body body |> List.assoc "data" in
      let ts = now () in
      let key = [sourceid; ts] in
      let v = `O [
          "datasource_id", `String sourceid;
          "timestamp", `String ts;
          "data", data
        ] in
      Lwt.catch (fun () ->
          S.update s key v >>= fun () ->
          sourceid_updated ~sourceid data >>= fun _ ->
          let resp = C.Response.make ~status:`OK () in
          Lwt.return (resp, Cohttp_lwt_body.empty)) (fun exn ->
          let status = `Internal_server_error in
          let error = Printexc.to_string exn in
          Lwt.return @@ make_err_response ~status ~error ())
  | `POST when action <> None ->
      S.list s [sourceid] >>= fun ts_lt ->
      let tss =
        List.sort Pervasives.compare ts_lt
        |> List.rev_map (fun k -> List.rev k |> List.hd)
      in
      Logs_lwt.debug (fun m -> m "[ts] available tss: %s" (String.concat " " tss))
      >>= fun () ->
      let to_read =
        match action with
        | Some "latest" -> (try [List.hd tss] with _ -> [])
        | Some "since" ->
            let s =
              obj_of_body body
              |> List.assoc "startTimestamp"
              |> Ez.get_string
            in
            List.fold_left (fun acc t ->
                if t >= s then t :: acc else acc) [] tss
        | Some "range" ->
            let o = obj_of_body body in
            let s = List.assoc "startTimestamp" o |> Ez.get_string in
            let e = List.assoc "endTimestamp" o |> Ez.get_string in
            List.fold_left (fun acc t ->
                if t >= s && t <= e then t :: acc else acc) [] tss
        | _ -> []
      in
      Lwt_list.map_p (fun t ->
          let key = [sourceid; t] in
          S.read s key >|= Ez.value) to_read >>= fun vs ->
      let b = `A vs |> Ez.to_string in

      Logs_lwt.debug (fun m -> m "[ts] response body: %s" b) >>= fun () ->
      let body = Cohttp_lwt_body.of_string b in
      let resp = C.Response.make ~status:`OK () in
      Lwt.return (resp, body)
  | _ ->
      let status = `Not_implemented in
      let error = C.Code.string_of_method meth in
      Lwt.return @@ make_err_response ~status ~error ()
