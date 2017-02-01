open Lwt
open Secure_log

module Key = Irmin.Contents.String.Path
module Value = Irmin.Contents.String
module TP_store = Irmin_unix.Irmin_fs.RW(Key)(Value)

type tp_log = key * log
type tp_store = int * TP_store.t

type t =
  { tp_log         : tp_log
  ; tp_buffer_cnt  : int
  ; tp_buffer_size : int
  ; tp_log_store   : tp_store
  }


let create ~key ?(buf_size=50) ?(root=".") () =
  let key = key_of_cstruct key in
  let tp_log = key, new_log key in
  let tp_buffer_cnt = 0 in
  let tp_buffer_size = buf_size in

  let config = Irmin_unix.Irmin_fs.config ~root () in
  TP_store.create config >>= fun store ->
  let tp_log_store = 0, store in
  return {tp_log; tp_buffer_cnt; tp_buffer_size; tp_log_store}


let flush_log t =
  if t.tp_buffer_cnt = 0 then return t
  else begin
    let key, log = t.tp_log in
    let () = get_entries log |> validate in
    let () = validate_macs log key in

    let batch, store = t.tp_log_store in
    let k = sexp_of_key key |> Sexplib.Sexp.to_string in
    let v = sexp_of_log log |> Sexplib.Sexp.to_string in
    TP_store.update store [string_of_int batch; "key"] k >>= fun () ->
    TP_store.update store [string_of_int batch; "log"] v >>= fun () ->

    let key' = get_key log in
    let tp_log = key', new_log key' in
    let tp_buffer_cnt = 0 in
    let tp_log_store = succ batch, store in
    return {t with tp_log; tp_buffer_cnt; tp_log_store}
  end


let append t ~entry_type entry =
  let tp_log = fst t.tp_log, append entry_type entry (snd t.tp_log) in
  let tp_buffer_cnt = succ t.tp_buffer_cnt in

  let t = {t with tp_log; tp_buffer_cnt} in
  if tp_buffer_cnt >= t.tp_buffer_size then flush_log t
  else return t


let read_batch batch store =
  TP_store.read_exn store [string_of_int batch; "key"] >>= fun key ->
  TP_store.read_exn store [string_of_int batch; "log"] >>= fun log ->
  let key = key |> Sexplib.Sexp.of_string |> key_of_sexp in
  let log = log |> Sexplib.Sexp.of_string |> log_of_sexp in
  return (key, log)


let get_logs t ?(max=10) () =
  let rec cut acc n rst =
    if n = 0 then List.rev acc else
      match rst with
      | []        -> List.rev acc
      | hd :: rst -> cut (hd :: acc) (pred n) rst
  in
  let key, log = t.tp_log in
  let batch, store = t.tp_log_store in

  let rec aux acc key log cnt batch =
    let logs = decrypt_all log key in
    let len = List.length logs in

    if len >= cnt then
      return @@ (cut [] cnt logs) @ acc
    else if batch = -1 then
      return @@ logs @ acc
    else
      read_batch batch store >>= fun (key, log) ->
      aux (logs @ acc) key log (cnt - len) (pred batch)
  in

  aux [] key log max (pred batch)


let is_valide ?(all=false) t =
  let batch, store = t.tp_log_store in
  let rec validate_batches batch =
    if batch = -1 then return_true
    else
      read_batch batch store >>= fun (_, log) ->
      wrap1 validate (get_entries log) >>= fun () ->
      validate_batches (pred batch)
  in

  let f () =
    let log = snd t.tp_log in
    let entries = get_entries log in
    wrap1 validate entries >>= fun () ->
    if all = false then return_true
    else validate_batches (pred batch)
  in
  let exn = function
    | Invalid_log -> return_false
  in
  catch f exn


let is_macs_valide ?(all=false) t =
  let batch, store = t.tp_log_store in
  let rec validate_macs_batches batch =
    if batch = -1 then return_true
    else
      read_batch batch store >>= fun (key, log) ->
      wrap2 validate_macs log key >>= fun () ->
      validate_macs_batches (pred batch)
  in

  let f () =
    let key, log = t.tp_log in
    wrap2 validate_macs log key >>= fun () ->
    if all = false then return_true
    else validate_macs_batches (pred batch)
  in
  let exn = function
    | Invalid_log -> return_false
  in
  catch f exn
