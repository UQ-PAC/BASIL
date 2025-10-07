#!/usr/bin/bash

eval $(../docker-helper.sh env)

export GTIRBSEM_FLAGS=""

OPTIONS="-DVERSION="\\\"0.94beta1\\\"" -std=c99 -D__BSD_VISIBLE -D_ALL_SOURCE -D_XOPEN_SOURCE=600 -D_POSIX_C_SOURCE=200112 -D_ISOC99_SOURCE -D_REENTRANT -D_BSD_SOURCE -D_DEFAULT_SOURCE -D_DARWIN_C_SOURCE  -Wall -Wextra -pedantic -Wshadow -Wcast-qual -Wbad-function-cast -Wstrict-prototypes -Wno-overlength-strings "
SOURCES="src/acl.c src/auth.c  src/config.c  src/direct.c  src/forward.c  src/http.c  src/main.c  src/ntlm.c src/pages.c src/proxy.c  src/scanner.c  src/socket.c  src/utils.c  src/xcrypt.c"

export CFLAGS="-O2 $OPTIONS"
CFILE_NAME="$SOURCES" ../liftmake.sh -b cntlm-noduk

export CFLAGS="-O0 $OPTIONS"
CFILE_NAME="$SOURCES" ../liftmake.sh -b cntlm-noduk_O0
