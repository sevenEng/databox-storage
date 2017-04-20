val init : ?secret:string -> unit -> unit Lwt.t

val macaroon_request_checker : Cohttp.Request.t -> (unit, Rresult.R.msg) result Lwt.t
