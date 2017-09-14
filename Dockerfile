FROM ocaml/opam:alpine-3.3_ocaml-4.03.0 as builder

RUN sudo apk add m4
RUN opam depext conf-m4.1
RUN opam install "core_kernel>=v0.9.0" "menhir>=20170607"

RUN sudo mkdir -m a=rwx /lambdasylum
WORKDIR /lambdasylum

COPY Makefile .
COPY jbuild .
COPY *.ml* ./

RUN sudo apk add coreutils

RUN eval `opam config env` && make test && make repl-build

RUN sudo mkdir -m a=rwx /rlwrap
WORKDIR /rlwrap
RUN wget https://github.com/hanslub42/rlwrap/releases/download/v0.43/rlwrap-0.43.tar.gz
RUN tar xf rlwrap-0.43.tar.gz
WORKDIR /rlwrap/rlwrap-0.43

RUN sudo apk add readline-dev
RUN ./configure
RUN make

FROM alpine:3.3
RUN apk update && apk add readline python3

WORKDIR /root/
COPY rlwrap-replace-escaped-lambdas.py .
COPY lambda-rlwrap.sh .
COPY --from=builder /lambdasylum/_build/default/repl.exe .
COPY --from=builder /rlwrap/rlwrap-0.43/src/rlwrap /usr/bin
COPY --from=builder /rlwrap/rlwrap-0.43/filters/rlwrapfilter.py .

ENV LANG=C.UTF-8

CMD sleep 0.1; ./lambda-rlwrap.sh ./repl.exe
