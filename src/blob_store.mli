open Store

module type BLOB_STORE = S with
    type key = Irmin.Contents.Cstruct.Path.t and
    type value = Irmin.Contents.Cstruct.t

module Blob_Store : BLOB_STORE
