#! /bin/sh

sudo apk add libsodium-dev
sudo apk add m4 perl libffi-dev

opam init
eval `opam config env`
opam install -y depext
opam depext -y conf-gmp.1 camlzip

opam pin add -n sodium https://github.com/sevenEng/ocaml-sodium.git#with_auth_hmac256
opam pin add -n macaroons https://github.com/nojb/ocaml-macaroons.git
opam pin add -n depyt https://github.com/sevenEng/depyt.git#fix-opam
opam pin add -n opium https://github.com/sevenEng/opium.git#fix-ssl-option
opam pin add -n secure_log_lib https://github.com/sevenEng/secure_log_lib.git

opam pin add -y databox-irmin-store .

mv /home/databox/.opam/system/bin/databox-irmin-store /home/databox/databox-irmin-store
rm -rf /home/databox/.opam

sudo apk del m4 perl libffi-dev
