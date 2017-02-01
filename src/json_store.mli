open Store

module type JSON_STORE = S with
    type key = Irmin.Contents.Json.Path.t and
    type value = Irmin.Contents.Json.t

module JSON_Store : JSON_STORE
