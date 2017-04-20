type tp_log

type tp_store

type t = {
  tp_log : tp_log;
  tp_buffer_cnt : int;
  tp_buffer_size : int;
  tp_log_store : tp_store;
}


val create : key:Cstruct.t -> ?buf_size:int -> ?root:string -> unit -> t Lwt.t

val flush_log : t -> t Lwt.t

val append : t -> entry_type:Cstruct.t -> Cstruct.t -> t Lwt.t

val get_logs : t -> ?max:int -> unit -> Cstruct.t list Lwt.t

val is_valide : ?all:bool -> t -> bool Lwt.t

val is_macs_valide : ?all:bool -> t -> bool Lwt.t
