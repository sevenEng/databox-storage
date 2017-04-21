type subject = [`Key of string | `Sourceid of string]

val register : Conduit_lwt_unix.flow -> Cohttp.Request.t
  -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

val handler : Store_engine.JSON_Store.t * Store_engine.JSON_Store.t
  -> clientid:string -> subject:subject -> action:string
  -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
