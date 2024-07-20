FROM fpco/stack-build:lts-18.28

ENV TERM xterm-256color
ENV LC_CTYPE C.UTF-8

RUN apt-get update && apt-get install -y nasm

RUN mkdir /home/stackage/yacll

VOLUME . /home/stackage/yacll

WORKDIR /home/stackage/yacll

RUN stack --resolver lts-18.28 build
