open Store

module type BLOB_STORE = S with
    type key = Irmin.Contents.Cstruct.Path.t and
    type value = Irmin.Contents.Cstruct.t

module XBLOB =
  Irmin_unix.Irmin_git.Memory
    (Irmin.Contents.Cstruct)
    (Irmin.Ref.String)
    (Irmin.Hash.SHA1)

module Blob_Store : BLOB_STORE = Store(XBLOB)
