CC=$(CLANG)
CFLAGS += -target $(TARGET) -fPIC
include $(GIT_ROOT)/src/test/make/lift.mk
