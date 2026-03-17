# Not working 
FROM ubuntu:23.04 as nix-container
RUN apt-get update && apt-get install --yes default-jre-headless python3 libgmp-dev yasm m4 \
  libcurl4-gnutls-dev pkg-config zlib1g-dev cmake ninja-build g++-10 \
  radare2 z3 libz3-dev llvm-14-dev \
  re2c \
  libpcre3-dev \
  xz-utils \
  clang-14 clang-15 gcc-aarch64-linux-gnu \
  wget \
  git \
  && apt-get autoremove --purge -y \
  && apt-get autoclean -y
RUN useradd -ms /bin/bash godbolt  
RUN mkdir -m 0755 /nix && chown godbolt /nix
USER godbolt
ENV USER godbolt
WORKDIR /home/godbolt
RUN wget --output-document=/dev/stdout https://nixos.org/nix/install | sh 
RUN . .nix-profile/etc/profile.d/nix.sh && nix-channel --update
RUN . .nix-profile/etc/profile.d/nix.sh \
  && nix-env -iA cachix -f https://cachix.org/api/v1/install  \
  && echo "trusted-users = root godbolt" | tee -a /nix/nix.conf \
  && cachix use pac-nix 
RUN . .nix-profile/etc/profile.d/nix.sh && nix-env -iA nixpkgs.boogie \
  && nix-channel --add https://github.com/katrinafyi/pac-nix/archive/refs/heads/main.tar.gz pac \
  && nix-channel --update \
  && nix-env -iA pac.bap-aslp

FROM nix-container AS compiler-explorer-tools
USER root
RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y ca-certificates gnupg \ 
   && mkdir -p /etc/apt/keyrings \
   && wget -q -S -O - https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg \
   && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_16.x nodistro main" > /etc/apt/sources.list.d/nodesource.list \
   && apt-get update \
   && apt-get install npm nodejs clang gcc gcc-13-aarch64-linux-gnu  binutils-aarch64-linux-gnu gcc-13-cross-base libc6-dev-arm64-cross libc6-dev-armel-cross libc6-dev-armhf-cross libc6-dev-i386 -y \
    && apt-get autoclean -y
ADD https://api.github.com/repos/ailrst/compiler-explorer/branches/main /tmp/head 
WORKDIR /compiler-explorer
USER root
RUN git clone https://github.com/ailrst/compiler-explorer.git /compiler-explorer \
    && chown -R godbolt:godbolt /compiler-explorer 
USER godbolt
RUN  cd /compiler-explorer \
    && echo "Add missing dependencies" \
    && npm i @sentry/node 
ENV PATH "$PATH:/home/godbolt/.nix-profile/bin"
ENTRYPOINT ["make"]
CMD ["run"]


