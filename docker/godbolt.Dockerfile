FROM ghcr.io/uq-pac/basil:latest as compiler-explorer
# https://github.com/madduci/docker-compiler-explorer/tree/master
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
    && apt-get install -y curl \
    && curl -sL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y \
        wget \
        ca-certificates \
        nodejs \
        make \
        git \
        dotnet6 \
    && dotnet tool install --global boogie  || true \
    && apt-get install clang gcc gcc-13-aarch64-linux-gnu  binutils-aarch64-linux-gnu gcc-13-cross-base libc6-dev-arm64-cross libc6-dev-armel-cross libc6-dev-armhf-cross libc6-dev-i386 -y \
    && apt-get autoremove --purge -y \
    && apt-get autoclean -y \
    && rm -rf /var/cache/apt/* /tmp/* 
# to force a new clone after a new commit
WORKDIR /compiler-explorer
ADD https://api.github.com/repos/ailrst/compiler-explorer/branches/main /tmp/head 
RUN  \
    git clone https://github.com/ailrst/compiler-explorer.git /compiler-explorer \
    && cd /compiler-explorer \
    && npm i @sentry/node \
    && npm run webpack
ENTRYPOINT [ "make" ]
CMD ["run"]

FROM compiler-explorer AS  ghcr.io/uq-pac/basil-compiler-explorer:latest
