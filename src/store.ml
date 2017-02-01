open Lwt

module type S = sig
  type t
  type key
  type value

  val create : ?msg:string -> key:Cstruct.t -> Irmin.config -> t Lwt.t

  val read   : t -> ?msg:string -> key -> value Lwt.t
  val update : t -> ?msg:string -> key -> value -> unit Lwt.t
  val remove : t -> ?msg:string -> key -> unit Lwt.t
  val list   : t -> ?msg:string -> key -> key list Lwt.t
  val remove_rec : t -> ?msg:string -> key -> unit Lwt.t
  end


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


  let create ?msg ~key config =
    Base.Repo.create config >>= fun repo ->
    Base.master Irmin_unix.task repo
    >>= fun data_store ->

    let root = "./.log" in
    Tp_log.create ~key ~root ()
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

  end
