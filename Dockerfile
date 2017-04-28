FROM alpine:3.5

LABEL distro_style="apk" distro="alpine" distro_long="alpine" arch="x86_64" ocaml_version="4.04.0" opam_version="1.2" operatingsystem="linux"

RUN apk update && apk upgrade \
 && apk add sudo \
 && adduser -S databox \
 && echo 'databox ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/databox \
 && chmod 440 /etc/sudoers.d/databox \
 && chown root:root /etc/sudoers.d/databox \
 && sed -i.bak 's/^Defaults.*requiretty//g' /etc/sudoers

USER databox
WORKDIR /home/databox

ADD . databox-irmin-store

RUN sudo apk add alpine-sdk bash ncurses-dev \
 && sudo apk add opam \
 && cd databox-irmin-store \
 && sudo chmod +x install.sh && sync

RUN cd databox-irmin-store \
 && ./install.sh

# && sudo apk del alpine-sdk bash ncurses-dev \
# && sudo apk del opam

EXPOSE 8080

LABEL databox.type="irmin-store"

CMD ["./irmin-store"]