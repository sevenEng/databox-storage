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

  val watch_key : t -> ?msg:string -> key -> (value Irmin.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
end


module Store (Base : Irmin.S): S
       with type key = Base.Key.t
        and type value = Base.Val.t


module type BLOB_STORE = S with
    type key = Irmin.Contents.Cstruct.Path.t and
    type value = Irmin.Contents.Cstruct.t

module Blob_Store : BLOB_STORE


module type JSON_STORE = S with
    type key = Irmin.Contents.Json.Path.t and
    type value = Irmin.Contents.Json.t

module JSON_Store : JSON_STORE
