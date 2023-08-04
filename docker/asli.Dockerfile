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
FROM aslp AS bap-upstream.2.5
USER opam
WORKDIR /home/opam
RUN opam depext --update --install bap.2.5.0 --yes -j 1
RUN opam install bap.2.5.0 --yes -j 1 \
 && opam clean -acrs 
USER root

# ====================
# Bap with ASLi plugin
# ====================
FROM bap-upstream.2.5 AS aslp-bap
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
  clang-14 gcc-aarch64-linux-gnu \
  && apt-get clean
# asli

# ==================
# Transplant bap: 
# ------------------
 COPY --from=aslp-bap /home/opam/.opam/4.14/bin /home/opam/.opam/4.14/bin
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib  /home/opam/.opam/4.14/lib
 COPY --from=aslp-bap /home/opam/.opam/4.14/share /home/opam/.opam/4.14/share
 COPY --from=aslp-bap /home/opam/aslp/mra_tools /aslp/mra_tools
 COPY --from=aslp-bap /home/opam/aslp/tests /aslp/tests
 COPY --from=aslp-bap /home/opam/aslp/asli /aslp/asli 
 COPY --from=aslp-bap /home/opam/aslp/prelude.asl /aslp/prelude.asl
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib/z3 /usr/local/lib/z3
 # opam env
 ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
 ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/4.14'
 ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/4.14/lib/toplevel'
 ENV ASLI_PATH=/aslp/
 ENV PATH=$PATH:/home/opam/.opam/4.14/bin
# ------------------
# Transplanted BAP 
# ==================

WORKDIR /basil
ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
ENV ASLI_PATH=/aslp/
ENV PATH=$PATH:/home/opam/.opam/4.14/bin
ADD scripts/docker/* /bin

# =========
# BASIL Jar 
# =========
FROM ubuntu:23.04 as basil:jar
RUN apt-get update && apt-get install default-jre-headless curl --yes 
COPY --from=basil /basil/target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar /basil/
ADD scripts/docker/* /bin
ENTRYPOINT java -jar /basil/wptool-boogie-assembly-0.0.1.jar


# =============
# Minimal image
# =============
FROM ubuntu:23.04 as minified-all 
RUN apt-get update && apt-get install --yes default-jre-headless python3 libgmp-dev yasm m4 \
  libcurl4-gnutls-dev pkg-config zlib1g-dev cmake ninja-build g++-10 \
  radare2 z3 libz3-dev llvm-14-dev \
  re2c \
  libpcre3-dev \
  clang-14 gcc-aarch64-linux-gnu \
  && apt-get clean
# ==================
# Transplant bap: 
# ------------------
 COPY --from=aslp-bap /home/opam/.opam/4.14/bin /home/opam/.opam/4.14/bin
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib  /home/opam/.opam/4.14/lib
 COPY --from=aslp-bap /home/opam/.opam/4.14/share /home/opam/.opam/4.14/share
 COPY --from=aslp-bap /home/opam/aslp/mra_tools /aslp/mra_tools
 COPY --from=aslp-bap /home/opam/aslp/tests /aslp/tests
 COPY --from=aslp-bap /home/opam/aslp/asli /aslp/asli 
 COPY --from=aslp-bap /home/opam/aslp/prelude.asl /aslp/prelude.asl
 COPY --from=aslp-bap /home/opam/.opam/4.14/lib/z3 /usr/local/lib/z3
 # opam env
 ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/4.14/lib/stublibs:/home/opam/.opam/4.14/lib/ocaml/stublibs:/home/opam/.opam/4.14/lib/ocaml'
 ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/4.14'
 ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/4.14/lib/toplevel'
 ENV ASLI_PATH=/aslp/
 ENV PATH=$PATH:/home/opam/.opam/4.14/bin
# ------------------
# Transplanted BAP 
# ==================
COPY --from=basil /basil/target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar /target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar
ADD scripts/docker/* /bin
