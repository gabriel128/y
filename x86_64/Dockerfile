FROM fpco/stack-build:lts-22.33

ENV TERM xterm-256color
ENV LC_CTYPE C.UTF-8

RUN apt-get update && apt-get install -y nasm

RUN mkdir /home/stackage/yacll

VOLUME . /home/stackage/yacll

WORKDIR /home/stackage/yacll

CMD tail -f /dev/null
