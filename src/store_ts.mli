val handler : Store_engine.JSON_Store.t -> Cohttp.Code.meth
  -> sourceid:string -> ?action:string -> body:string -> unit
  -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
