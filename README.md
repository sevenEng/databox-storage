databox-storage — databox's storage system component
-------------------------------------------------------------------------------
%%VERSION%%

databox-storage is TODO

databox-storage is distributed under the ISC license.

Homepage: https://github.com/sevenEng/databox-storage  

## Installation

databox-storage can be installed with `opam`:

    opam install databox-storage

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
databox-storage`.

[doc]: https://www.cl.cam.ac.uk/~ql272/databox-storage/doc

## Sample programs

If you installed databox-storage with `opam` sample programs are located in
the directory `opam var databox-storage:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 
