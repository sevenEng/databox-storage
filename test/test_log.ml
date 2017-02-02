open Tp_log
open Alcotest
open Lwt


let eval t = Lwt_main.run t

let new_log ?buf_size ?root () =
  let key = "encryption key" |> Cstruct.of_string in
  let root = "/tmp/databox" in
  create ~key ?buf_size ~root ()

let entry_type = Cstruct.of_string "test"

let entries = [
    "oh";
    "i'm a log";
    "yeah?";
    "i'm a log too";
  ]

let more_entries = [
    "what?";
    "you are no log";
    "why? I could be one"
  ]



let append_list l e = e
  |> List.map Cstruct.of_string
  |> Lwt_list.fold_left_s (fun l e ->
         append l ~entry_type e) l


let rw () =
  let thrd =
    new_log () >>= fun l ->
    append_list l entries >>= fun l ->
    get_logs l () >>= fun entries ->
    entries
    |> List.map Cstruct.to_string
    |> return
  in

  let expected = List.rev entries in
  let entries' = eval thrd in
  check (list string) "r/w entries" expected entries'


let rw_with_flush () =
  let thrd =
    (*flush happens when number of entries exceeds 5*)
    new_log ~buf_size:5 () >>= fun l ->
    append_list l entries >>= fun l ->
    append_list l more_entries >>= fun l ->

    get_logs l () >>= fun entries ->
    entries
    |> List.map Cstruct.to_string
    |> return
  in

  let expected = List.rev (entries @ more_entries) in
  let entries' = eval thrd in
  check (list string) "rw_flush entries" expected entries'


let rw_with_flush_and_cut () =
  let thrd =
    (*flush happens when number of entries exceeds 3*)
    new_log ~buf_size:3 () >>= fun l ->
    append_list l entries >>= fun l ->
    append_list l more_entries >>= fun l ->

    get_logs ~max:5 l () >>= fun entries ->
    entries
    |> List.map Cstruct.to_string
    |> return
  in

  let rec cut acc n rst =
    if n = 0 then List.rev acc else
      match rst with
      | []        -> List.rev acc
      | hd :: rst -> cut (hd :: acc) (pred n) rst
  in

  let expected = List.rev more_entries @  (List.rev entries |> cut [] 2)in
  let entries' = eval thrd in
  check (list string) "rw_flush_cut entries" expected entries'


let basic_rw = [
  "rw in memory", `Slow, rw;
  "rw with persisted entries", `Slow, rw_with_flush;
  "rw with persisted entries being cut", `Slow, rw_with_flush_and_cut
]


let () =
  run "test of tp_log" [
    "basic rw", basic_rw;
  ]
