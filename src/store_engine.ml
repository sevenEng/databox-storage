open Lwt

module type S = sig
  type t
  type key
  type value

  val create : ?msg:string -> ?log_root:string -> log_key:Cstruct.t -> Irmin.config -> t Lwt.t

  val read   : t -> ?msg:string -> key -> value Lwt.t
  val update : t -> ?msg:string -> key -> value -> unit Lwt.t
  val remove : t -> ?msg:string -> key -> unit Lwt.t
  val list   : t -> ?msg:string -> key -> key list Lwt.t
  val remove_rec : t -> ?msg:string -> key -> unit Lwt.t

  val logs   : t -> ?msg:string -> ?max:int -> unit -> string list Lwt.t
  val is_valide : t -> ?msg:string -> ?all:bool -> unit -> bool Lwt.t
  end

module Tp_log = Store_log

module Store (Base : Irmin.S) = struct
  type t =
    { data_store : string -> Base.t
    ; mutable log_store  : Tp_log.t
    }
  type key = Base.key
  type value = Base.value

  let store_log = Cstruct.of_string "STORE_LOG"
  let user_log  = Cstruct.of_string "USER_LOG"

  let append_store_log msg log =
    let msg = Cstruct.of_string msg in
    Tp_log.append log ~entry_type:store_log msg

  let append_user_log ?msg log =
    match msg with
    | None -> return log
    | Some msg ->
       let msg = Cstruct.of_string msg in
       Tp_log.append log ~entry_type:user_log msg

  let read_task store =
    Base.task store
    |> Irmin.Task.to_json
    |> Ezjsonm.wrap
    |> Ezjsonm.to_string ~minify:true


  let create ?msg ?log_root ~log_key config =
    Base.Repo.create config >>= fun repo ->
    Base.master Irmin_unix.task repo
    >>= fun data_store ->

    let root = match log_root with
      | None -> "/tmp/databox_log"
      | Some r -> r
    in
    Tp_log.create ~key:log_key ~root ()
    >>= append_store_log "create store"
    >>= append_user_log ?msg
    >>= fun log_store ->

    return {data_store; log_store}


  let read t ?msg k =
    let task = Printf.sprintf "read %s" (Base.Key.to_hum k) in
    let store = t.data_store task in
    Base.read_exn store k >>= fun v ->

    append_store_log (read_task store) t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    return v


  let update t ?msg k v =
    let task = Printf.sprintf "update %s with %d"
                              (Base.Key.to_hum k) (Base.Val.hash v) in
    let store = t.data_store task in
    Base.update store k v >>= fun () ->

    append_store_log (read_task store) t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    return_unit


  let remove t ?msg k =
    let task = Printf.sprintf "remove %s" (Base.Key.to_hum k) in
    let store = t.data_store task in
    Base.remove store k >>= fun () ->

    append_store_log (read_task store) t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    return_unit


  let list t ?msg k =
    let task = Printf.sprintf "list sub-keys under %s" (Base.Key.to_hum k) in
    let store = t.data_store task in
    Base.list store k >>= fun sub_ks ->

    append_store_log (read_task store) t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    return sub_ks


  let remove_rec t ?msg k =
    let task = Printf.sprintf "remove_rec for %s" (Base.Key.to_hum k) in
    let store = t.data_store task in
    Base.remove_rec store k >>= fun () ->

    append_store_log (read_task store) t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    return_unit


  let logs t ?msg ?(max=10) () =
    Tp_log.get_logs t.log_store ~max () >>= fun logs ->

    let store_log = "read logs with max = " ^ (string_of_int max) in
    append_store_log store_log t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    List.map Cstruct.to_string logs
    |> return


  let is_valide t ?msg ?(all=false) () =
    Tp_log.is_valide ~all t.log_store >>= fun log_validity ->
    Tp_log.is_macs_valide ~all t.log_store >>= fun macs_validity ->

    let store_log = "check validity with all = " ^ (string_of_bool all) in
    append_store_log store_log t.log_store
    >>= append_user_log ?msg
    >>= fun log_store ->

    t.log_store <- log_store;
    return (log_validity && macs_validity)
  end



module type JSON_STORE = S with
    type key = Irmin.Contents.Json.Path.t and
    type value = Irmin.Contents.Json.t

module XJSON =
  Irmin_unix.Irmin_git.Memory
    (Irmin.Contents.Json)
    (Irmin.Ref.String)
    (Irmin.Hash.SHA1)

module JSON_Store : JSON_STORE = Store(XJSON)


module type BLOB_STORE = S with
    type key = Irmin.Contents.Cstruct.Path.t and
    type value = Irmin.Contents.Cstruct.t

module XBLOB =
  Irmin_unix.Irmin_git.Memory
    (Irmin.Contents.Cstruct)
    (Irmin.Ref.String)
    (Irmin.Hash.SHA1)

module Blob_Store : BLOB_STORE = Store(XBLOB)
