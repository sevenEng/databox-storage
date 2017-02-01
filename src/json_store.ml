open Store

module type JSON_STORE = S with
    type key = Irmin.Contents.Json.Path.t and
    type value = Irmin.Contents.Json.t

module XJSON =
  Irmin_unix.Irmin_git.Memory
    (Irmin.Contents.Json)
    (Irmin.Ref.String)
    (Irmin.Hash.SHA1)

module JSON_Store : JSON_STORE = Store(XJSON)
