FROM fpco/stack-build:lts-22.33 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10 nasm
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/

RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------

FROM haskell:9.6.6 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack

WORKDIR /opt/build

RUN apt-get update && apt-get install -y nasm

CMD tail -f /dev/null
