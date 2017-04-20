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


let handler s meth ~sourceid ?action ~body () =
  match meth with
  | `POST ->
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
          let resp = C.Response.make ~status:`OK () in
          Lwt.return (resp, Cohttp_lwt_body.empty)) (fun exn ->
          let status = `Internal_server_error in
          let error = Printexc.to_string exn in
          Lwt.return @@ make_err_response ~status ~error ())
  | `GET ->
      S.list s [sourceid] >>= fun ts_lt ->
      let tss =
        List.sort Pervasives.compare ts_lt
        |> List.rev_map (fun k -> List.rev k |> List.hd)
      in
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
      let body = Cohttp_lwt_body.of_string b in
      let resp = C.Response.make ~status:`OK () in
      Lwt.return (resp, body)
  | _ ->
      let status = `Not_implemented in
      let error = C.Code.string_of_method meth in
      Lwt.return @@ make_err_response ~status ~error ()
