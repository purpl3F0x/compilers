CC ?= clang-18
CLANG ?= clang-18
CFLAGS += -Wall -Werror -O3 -ffast-math -std=gnu17 -I.
LIB_NAME = libalan.a
TARGET_TRIPLE ?= $(shell llvm-config --host-target)

OUT_DIR ?= .

# Source and object files
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)

ifeq ($(CC), clang)
	CFLAGS += -Wno-incompatible-library-redeclaration --target=$(TARGET_TRIPLE)
else
	CFLAGS += -Wno-builtin-declaration-mismatch
endif

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
	$(CC) $(CFLAGS) -c $< -o $@

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
