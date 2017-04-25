open Lwt

module Client = Cohttp_lwt_unix.Client
module Ez     = Ezjsonm
module Mrn    = Sodium_macaroons

let serve_scheme = "http"
let serve_port = 8080
let serve_uri =
  Uri.of_string "//127.0.0.1"
  |> fun t -> Uri.with_scheme t (Some serve_scheme)
  |> fun t -> Uri.with_port t (Some serve_port)


let mr_secret = "test-secret"
let test_mrn ~meth ~path =
  let location = Store_env.local_name () in
  let key = mr_secret in
  let id = "testeur" in
  let m = Mrn.create ~location ~key ~id in
  let m_t = Mrn.add_first_party_caveat m ("target = " ^ (Store_env.local_name ())) in
  let m_tm = Mrn.add_first_party_caveat m_t ("method = " ^ (Cohttp.Code.string_of_method meth)) in
  let m_tmp = Mrn.add_first_party_caveat m_tm ("path = " ^ path) in
  m_tmp

let with_mrn_header ~meth ~path =
  let m = test_mrn ~meth ~path in
  Cohttp.Header.init_with "x-api-key" (Mrn.serialize m)

let body_of_obj obj =
  obj |> Ez.to_string
  |> Cohttp_lwt_body.of_string

let obj_of_body b =
  Cohttp_lwt_body.to_string b >>= fun b ->
  Ez.from_string b
  |> Ez.value
  |> Ez.get_dict
  |> return

let assert_equal exp rel ?to_string () =
  if exp <> rel then begin
    (match to_string with
    | Some fn ->
        Logs_lwt.debug (fun m -> m "[test] EXP:%s <> REL:%s" (fn exp) (fn rel))
    | None ->
        Logs_lwt.debug (fun m -> m "[test] assert fails"))
    >>= fun () ->
    Lwt.fail (Failure "assertion")
  end
  else return_unit


let kv_rw () =
  let path = "/test_kv/kv" in
  let uri = Uri.with_path serve_uri path in
  let obj = `O ["key", `String "value"; "options", `A [`Float 3.14; `Null]] in
  let body = body_of_obj obj in

  Client.post ~headers:(with_mrn_header ~meth:`POST ~path) ~body uri >>= fun _ ->
  Client.get ~headers:(with_mrn_header ~meth:`GET ~path) uri >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun b ->
  let obj' = Ez.from_string b in
  assert_equal obj obj' ~to_string:Ez.to_string ()


let now () =
  Ptime_clock.now ()
  |> Ptime.to_float_s
  |> (fun s -> s *. 1000.)
  |> int_of_float
  |> string_of_int


let ts_latest () =
  let sourceid = "test_latest" in
  let w_path = "/" ^ sourceid ^ "/ts" in
  let w_uri = Uri.with_path serve_uri w_path in
  let data = `O ["data", `String "latest_data"] in
  let body = body_of_obj data in
  Client.post ~headers:(with_mrn_header ~meth:`POST ~path:w_path) ~body w_uri >>= fun _ ->

  let path = "/" ^ sourceid ^ "/ts/latest" in
  let uri = Uri.with_path serve_uri path in
  Client.post ~headers:(with_mrn_header ~meth:`POST ~path) uri >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun b ->

  Ez.from_string b
  |> Ez.get_list (fun v ->
      let obj' = Ez.get_dict v in
      let sourceid' =
        List.assoc "datasource_id" obj'
        |> Ez.get_string
      in
      assert (sourceid = sourceid');
      `O ["data", List.assoc "data" obj'])
  |> Array.of_list
  |> fun arr -> begin
    assert (1 = Array.length arr);
    assert_equal data arr.(0) ~to_string:(fun v -> Ez.(wrap v |> to_string)) ()
  end


let ts_since () =
  let sourceid = "test_since" in
  let w_path = "/" ^ sourceid ^ "/ts" in
  let w_uri = Uri.with_path serve_uri w_path in
  let w_header = with_mrn_header ~meth:`POST ~path:w_path in

  let data =
    Array.init 3 (fun inx -> `O ["data", `String (Printf.sprintf "since_data_%d" inx)])
  in
  Client.post ~headers:w_header ~body:(body_of_obj data.(0)) w_uri >>= fun _ ->
  Lwt_unix.sleep 0.05 >>= fun () ->
  let start_ts = now () in
  Client.post ~headers:w_header ~body:(body_of_obj data.(1)) w_uri >>= fun _ ->
  Client.post ~headers:w_header ~body:(body_of_obj data.(2)) w_uri >>= fun _ ->

  let path = "/" ^ sourceid ^ "/ts/since" in
  let uri = Uri.with_path serve_uri path in
  let body = `O ["startTimestamp", `String start_ts] |> body_of_obj in
  Client.post ~headers:(with_mrn_header ~meth:`POST ~path) ~body uri >>= fun (_, body) ->

  Cohttp_lwt_body.to_string body >>= fun b ->
  Ez.from_string b
  |> Ez.get_list (fun v ->
      let obj' = Ez.get_dict v in
      let sourceid' =
        List.assoc "datasource_id" obj'
        |> Ez.get_string
      in
      assert (sourceid = sourceid');
      `O ["data", List.assoc "data" obj'])
  |> Array.of_list
  |> fun arr -> begin
    assert_equal 2 (Array.length arr) ~to_string:string_of_int () >>= fun () ->
    assert_equal data.(1) arr.(0) ~to_string:(fun v -> Ez.to_string v) () >>= fun () ->
    assert_equal data.(2) arr.(1) ~to_string:(fun v -> Ez.to_string v) () >>= fun () ->
    (*not sure about the order of returned data*)
    return_unit
    end


let ts_range () =
  let sourceid = "test_range" in
  let w_path = "/" ^ sourceid ^ "/ts" in
  let w_uri = Uri.with_path serve_uri w_path in
  let w_header = with_mrn_header ~meth:`POST ~path:w_path in

  let data =
    Array.init 3 (fun inx -> `O ["data", `String (Printf.sprintf "range_data_%d" inx)])
  in
  Client.post ~headers:w_header ~body:(body_of_obj data.(0)) w_uri >>= fun _ ->
  Lwt_unix.sleep 0.1 >>= fun () ->
  let start_ts = now () in
  Client.post ~headers:w_header ~body:(body_of_obj data.(1)) w_uri >>= fun _ ->
  Lwt_unix.sleep 0.1 >>= fun () ->
  let end_ts = now () in
  Client.post ~headers:w_header ~body:(body_of_obj data.(2)) w_uri >>= fun _ ->

  let path = "/" ^ sourceid ^ "/ts/range" in
  let uri = Uri.with_path serve_uri path in
  let body = `O [
      "startTimestamp", `String start_ts;
      "endTimestamp", `String end_ts;
    ] |> body_of_obj
  in
  Client.post ~headers:(with_mrn_header ~meth:`POST ~path) ~body uri >>= fun (_, body) ->

  Cohttp_lwt_body.to_string body >>= fun b ->
  Ez.from_string b
  |> Ez.get_list (fun v ->
      let obj' = Ez.get_dict v in
      let sourceid' =
        List.assoc "datasource_id" obj'
        |> Ez.get_string
      in
      assert (sourceid = sourceid');
      `O ["data", List.assoc "data" obj'])
  |> Array.of_list
  |> fun arr -> begin
    assert_equal 1 (Array.length arr) ~to_string:string_of_int () >>= fun () ->
    assert_equal data.(1) arr.(0) ~to_string:Ez.to_string () >>= fun () ->
    return_unit
  end


let read_and_print read () =
  let rec aux () =
    read () >>= fun fr ->
    Logs_lwt.debug (fun m -> m "[test] read frame %s" (Websocket_lwt.Frame.show fr))
    >>= aux
  in
  aux ()


let ws_kv () =
  let path = "/ws" in
  let uri = Uri.with_path serve_uri path in
  let extra_headers = with_mrn_header ~meth:`POST ~path in
  let client = `TCP (`IP (Ipaddr.of_string_exn "127.0.0.1"), `Port serve_port) in
  Websocket_lwt.with_connection ~extra_headers ~ctx:Conduit_lwt_unix.default_ctx client uri
  >>= fun (read_fr, _) ->
  let key = "test_ws_kv" in
  let sub_path = "/sub/" ^ key ^ "/kv" in
  let sub_uri = Uri.with_path serve_uri sub_path in
  Client.post ~headers:(with_mrn_header ~meth:`POST ~path:sub_path)  sub_uri >>= fun _ ->

  let obj = `O ["data", `String "ws_kv_data"] in
  let body = body_of_obj obj in
  let w_path = "/" ^ key ^ "/kv" in
  let w_uri = Uri.with_path serve_uri w_path in
  let assert_t () =
    read_fr () >>= fun fr' ->
    let fr = Websocket_lwt.Frame.(create ~opcode:Opcode.Text ~content:(Ez.to_string obj) ()) in
    assert_equal fr fr' ~to_string:Websocket_lwt.Frame.show ()
  in

  Lwt.join [
    assert_t ();
    Client.post ~headers:(with_mrn_header ~meth:`POST ~path:w_path) ~body w_uri
    >>= fun _ -> return_unit]



let test_store () =
  let kv = "test kv store", ["rw", kv_rw] in
  let ts = "test ts store", ["latest", ts_latest; "since", ts_since; "range", ts_range] in
  let ws = "test ws", ["ws_kv", ws_kv] in
  let success = ref true in
  let failure_cases = ref [] in
  Lwt_list.iter_s (fun (s, suit) ->
      Logs_lwt.info (fun m -> m "[test] begin test suit %s" s) >>= fun () ->
      Lwt_list.iter_s (fun (c, case) ->
          Lwt.catch (fun () ->
              case () >>= fun () ->
              (*if succeeds*)
              Logs_lwt.info (fun m -> m "[test] test case %s SUCCEEDED" c)) (fun exn ->
              (*if fails*)
              success := false;
              failure_cases := c :: !failure_cases;
              let f = Printexc.to_string exn in
              Logs_lwt.err (fun m -> m "[test] test case %s FAILED %s" c f))) suit >>= fun () ->
      Logs_lwt.info (fun m -> m "[test] test suite %s ended" s)) [kv; ts; ws]
  >>= fun () ->
  if !success then return_unit
  else begin
    let cases = String.concat " " !failure_cases in
    Logs_lwt.err (fun m -> m "[test] test case(s) didn't pass: [%s]" cases) >>= fun () ->
    Lwt.fail (Failure "TEST FAIL")
  end


let test_main () =
  let secret = mr_secret in
  let port = serve_port in
  Lwt.choose [
    Databox_irmin_store.main ~secret ~port ();
    Lwt_unix.sleep 1.0 >>= test_store;
  ]


let () =
  Logs.(set_level (Some Debug));
  let () = List.iter (fun src ->
      let n = Logs.Src.name src in
      if Astring.String.is_prefix ~affix:"irmin" n ||
         Astring.String.is_prefix ~affix:"git" n ||
         Astring.String.equal "store.macaroon" n then begin
        Logs.(Src.set_level src (Some Error)) end
      else ()
    ) @@ Logs.Src.list () in
  Logs.set_reporter (Logs_fmt.reporter ());
  Lwt_main.run @@ test_main ()
