CC=$(CLANG)
CFLAGS += -target $(TARGET) -fno-plt -fno-pic
include $(GIT_ROOT)/src/test/make/lift.mk
