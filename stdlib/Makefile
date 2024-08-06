CC ?= gcc
CFLAGS += -Wall -Werror -O3 --fast-math -std=gnu17 -march=x86-64-v2 -mtune=haswell -I.
LIB_NAME = libalan.a

OUT_DIR ?= .

# Source and object files
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)

ifeq ($(CC), clang)
	CFLAGS += -Wno-incompatible-library-redeclaration 
else
	CFLAGS += -Wno-builtin-declaration-mismatch 
endif

# Default target
all: $(OUT_DIR)/$(LIB_NAME)

# Create the static library
$(OUT_DIR)/$(LIB_NAME): $(OBJ)
	@mkdir -p $(OUT_DIR)
	@echo "Creating static library $@"
	ar rcs -o $@ $^
	rm -f *.o

# Compile each .c file to .o
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up build files
clean:
	@echo "Cleaning up..."
	rm -f *.o

distclean: clean
	rm -f $(OUT_DIR)/$(LIB_NAME)


.PHONY: all clean
