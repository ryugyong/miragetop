#!/bin/bash
set -x

docker build -t miragetop - <<EOF
FROM alpine:3.9

RUN wget http://caml.inria.fr/pub/distrib/ocaml-4.07/ocaml-4.07.1.tar.gz && \
    tar xvf ocaml-4.07.1.tar.gz

RUN apk add bash curl ca-certificates git gcc g++ libc-dev make

RUN cd ocaml-4.07.1/ && \
    ./configure -with-debug-runtime -no-graph && \
    make world.opt && \
    make ocamlnat && \
    make install

RUN wget https://github.com/ocaml/opam/releases/download/2.0.4/opam-full-2.0.4.tar.gz && \
    tar xvf opam-full-2.0.4.tar.gz

RUN cd opam-full-2.0.4/ && \
    ./configure && \
    make lib-ext && \
    make && \
    make install && \
    opam init && \
    opam install mirage

EOF

