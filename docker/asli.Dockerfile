# ====
# ASLP
# ====
FROM ocaml/opam:ubuntu-23.04-ocaml-4.14 AS aslp
USER root

# Install system dependencies
RUN apt-get update && apt-get install -y python3 libgmp-dev yasm m4 \
  libcurl4-gnutls-dev pkg-config zlib1g-dev cmake ninja-build g++-10 \
  radare2 z3 libz3-dev llvm-14-dev \
  re2c \
  libpcre3-dev

USER opam
ENV OPAMROOT=/home/opam/.opam

# clone and install aslp
USER opam
RUN cd /home/opam && git clone https://github.com/UQ-PAC/aslp.git  
RUN eval $(opam config env) && cd /home/opam/aslp && opam install . --deps-only --with-test  -j1
RUN cd /home/opam/aslp && eval $(opam config env) && export LD_LIBRARY_PATH=`opam config var z3:lib` \
  && make install 
# so the aslp script is before aslp in the path 
ENV PATH=/home/opam/aslp:$PATH

# ============
# Bap Upstream 
# ============
# It would be more convenient to use the dpkg package, however 
# it requires old versions of libffi and libtinfo and generally 
# doesn't support ubuntu well.
# Opam install is the most reliable way.
FROM aslp AS bap-upstream.2.5
USER opam
WORKDIR /home/opam
RUN opam depext --update --install bap.2.5.0 --yes -j 1
RUN opam install bap.2.5.0 --yes -j 1 \
 && opam clean -acrs 
USER root

# ============
# Bap Pac 
# ============
# It would be more convenient to use the dpkg package, however 
# it requires old versions of libffi and libtinfo and generally 
# doesn't support ubuntu well.
# Opam install is the most reliable way.
FROM ocaml/opam:ubuntu-23.04-ocaml-4.14 AS aslp-bap
USER root
# Install system dependencies
RUN apt-get update && apt-get install -y python3 libgmp-dev yasm m4 \
  libcurl4-gnutls-dev pkg-config zlib1g-dev cmake ninja-build g++-10 \
  radare2 z3 libz3-dev llvm-14-dev \
  re2c \
  libpcre3-dev
USER opam
WORKDIR /home/opam
#RUN eval $(opam env) && opam pin add z3 4.8.7 --yes -n   
RUN eval $(opam env) && opam depext --install z3 -j1  # this is a separate stage since it takes a very long time to build
RUN eval $(opam env) \
 && opam pin add bap https://github.com/UQ-PAC/bap.git --yes -n \
 && opam pin add asli https://github.com/UQ-PAC/asl-interpreter.git --yes -n \ 
 && opam install --yes --deps-only bap
RUN git clone https://github.com/UQ-PAC/bap.git 
RUN cd bap && eval $(opam env) && opam install oasis \ 
    &&  opam install ./opam --deps-only  -j1 \
    && ./configure --enable-everything \
    --disable-ghidra --disable-radare2 --disable-primus-symbolic-executor \
    --prefix=`opam var prefix` \
    --with-llvm-version=14 --with-llvm-config=llvm-config-14 \
    && make && make reinstall \
 && opam clean -acrs 
USER root


# ====================
# Bap with ASLi plugin
# ====================
FROM bap-upsteam.2.5 AS aslp-bap-upstream
USER opam
RUN git clone https://github.com/UQ-PAC/bap-asli-plugin.git
RUN cd /home/opam/bap-asli-plugin && eval $(opam env) && make
ENV ASLI_PATH=/home/opam/aslp
USER root 

# ==================
# Transplant bap: 
# ------------------
# COPY --from=aslp-bap /home/opam/.opam/4.14/bin /home/opam/.opam/4.14/bin
# COPY --from=aslp-bap /home/opam/.opam/4.14/lib  /home/opam/.opam/4.14/lib
# COPY --from=aslp-bap /home/opam/.opam/4.14/share /home/opam/.opam/4.14/share
# COPY --from=aslp-bap /home/opam/aslp/mra_tools /aslp/mra_tools
# COPY --from=aslp-bap /home/opam/aslp/tests /aslp/tests
# COPY --from=aslp-bap /home/opam/aslp/asli /aslp/asli 
# COPY --from=aslp-bap /home/opam/aslp/prelude.asl /aslp/prelude.asl
# COPY --from=aslp-bap /home/opam/.opam/4.14/lib/z3 /usr/local/lib/z3
# # opam env
# ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
# ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/4.14'
# ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/4.14/lib/toplevel'
# ENV ASLI_PATH=/aslp/
# ENV PATH=$PATH:/home/opam/.opam/4.14/bin
# ------------------
# Transplanted BAP 
# ==================


# =======================
# BASIL build environment
# =======================
FROM ubuntu:23.04 as scala
ENV PATH="$PATH:/root/.local/share/coursier/bin"
RUN apt-get update && apt-get install default-jre-headless curl --yes \ 
 && curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs && chmod +x cs && ./cs setup --yes \
 && apt-get remove curl --yes && apt-get autoremove --yes && apt-get clean

# =============
# Compile BASIL
# =============
FROM scala as basil
ADD . /basil
RUN cd /basil && sbt assembly

# ===============
# BASIL Dev Image 
# ===============
FROM basil as basil:dev 
# use the basil image so sbt cache is full
RUN rm -rf /basil
RUN apt-get update && apt-get install --yes default-jre-headless python3 libgmp-dev yasm m4 \
  libcurl4-gnutls-dev pkg-config zlib1g-dev cmake ninja-build g++-10 \
  radare2 z3 libz3-dev llvm-14-dev \
  re2c \
  libpcre3-dev \
  clang-14 clang-15 gcc-aarch64-linux-gnu \
  dotnet6 \
  && apt-get clean \
  && dotnet tool install --global boogie
# asli

# ==================
# Transplant bap: 
# ------------------
 COPY --from=aslp-bap /home/opam/.opam/4.14/bin /home/opam/.opam/4.14/bin
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib  /home/opam/.opam/4.14/lib
 COPY --from=aslp-bap /home/opam/.opam/4.14/share /home/opam/.opam/4.14/share
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib/z3 /usr/local/lib/z3
 # opam env
 ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
 ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/4.14'
 ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/4.14/lib/toplevel'
 ENV ASLI_PATH=/aslp/
 ENV PATH=$PATH:/home/opam/.opam/4.14/bin:/root/.dotnet/tools/
# ------------------
# Transplanted BAP 
# ==================

WORKDIR /basil
ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
ENV ASLI_PATH=/aslp/
ENV PATH=$PATH:/home/opam/.opam/4.14/bin:/root/.dotnet/tools/


# =============
# Minimal image
# =============
FROM ubuntu:23.04 as minified-all 
RUN apt-get update && apt-get install --yes default-jre-headless python3 libgmp-dev yasm m4 \
  libcurl4-gnutls-dev pkg-config zlib1g-dev cmake ninja-build g++-10 \
  radare2 z3 libz3-dev llvm-14-dev \
  re2c \
  libpcre3-dev \
  clang-14 clang-15 gcc-aarch64-linux-gnu \
  dotnet6 \
  && apt-get clean \ 
  && dotnet tool install --global boogie
# ==================
# Transplant bap: 
# ------------------
 COPY --from=aslp-bap /home/opam/.opam/4.14/bin /home/opam/.opam/4.14/bin
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib  /home/opam/.opam/4.14/lib
 COPY --from=aslp-bap /home/opam/.opam/4.14/share /home/opam/.opam/4.14/share
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib/z3 /usr/local/lib/z3
 # opam env
 ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
 ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/4.14'
 ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/4.14/lib/toplevel'
 ENV PATH=$PATH:/home/opam/.opam/4.14/bin:/root/.dotnet/tools/
# ------------------
# Transplanted BAP 
# ==================
COPY --from=basil /basil/target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar /target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar
WORKDIR /app
