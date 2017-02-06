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


module Store (Base : Irmin.S): S
       with type key = Base.Key.t
        and type value = Base.Val.t
