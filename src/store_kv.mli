val handler : Store_engine.JSON_Store.t -> Cohttp.Code.meth
  -> key:string -> body:string
  -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
