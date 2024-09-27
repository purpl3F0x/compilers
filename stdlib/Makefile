# Try to find a suitable clang compiler
CLANG_VERSION = 18
CLANG_BIN ?= $(shell which clang-$(CLANG_VERSION) 2>/dev/null || which clang 2>/dev/null)
# Check clang version
CHECK_CLANG_VERSION = $(shell $(CLANG_BIN) --version | grep -oP 'clang version \K[0-9]+')
# Check if the located clang is version 18
ifeq ($(CHECK_CLANG_VERSION), $(CLANG_VERSION))
    CLANG = $(CLANG_BIN)
else
    $(error No suitable clang compiler found)
endif

CFLAGS += -Wall -Werror -O3 -ffast-math -std=gnu17 -I.
CFLAGS += -Wno-incompatible-library-redeclaration --target=$(TARGET_TRIPLE)

LIB_NAME = libalan.a
TARGET_TRIPLE ?= $(shell llvm-config --host-target)

OUT_DIR ?= .

# Source and object files
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)


ifeq ($(findstring x86_64,$(TARGET_TRIPLE)),x86_64)
  CFLAGS += -march=x86-64-v2 -mtune=skylake
endif

# Default target
.DEFAULT_GOAL := $(OUT_DIR)/$(LIB_NAME)

all: $(OUT_DIR)/$(LIB_NAME) llvm-ir

# Create the static library
$(OUT_DIR)/$(LIB_NAME): $(OBJ)
	@mkdir -p $(OUT_DIR)
	@echo "Creating static library $@"
	ar rcs -o $@ $^

# Compile each .c file to .o
%.o: %.c
	$(CLANG) $(CFLAGS) -c $< -o $@

# Compile each .c file to .ll
%.ll: %.c
	$(CLANG) -S -emit-llvm $< -o $@ -Wall -Werror -O3 -Wno-incompatible-library-redeclaration 

llvm-ir: $(SRC:.c=.ll)


# Clean up build files
clean:
	@echo "Cleaning up..."
	rm -f *.o *.ll

distclean: clean
	rm -f $(OUT_DIR)/$(LIB_NAME) *.


.PHONY: all clean
