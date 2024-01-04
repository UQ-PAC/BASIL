CC=$(CLANG)
CFLAGS += -target $(TARGET) -O2
include $(GIT_ROOT)/src/test/make/lift.mk
