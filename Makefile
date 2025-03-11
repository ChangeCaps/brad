# Project configuration
BINARY_NAME=brad

# Directories
RUST_SRC=$(shell find src -type f -name '*.rs')
RUNTIME_DIR=runtime
OBJ_DIR=obj

# Detect std_*.c files and other C files
STD_C_SRC=$(wildcard $(RUNTIME_DIR)/std_*.c)
STD_OBJ_NAMES=$(patsubst $(RUNTIME_DIR)/%.c,%.o,$(STD_C_SRC))

OTHER_C_SRC=$(filter-out $(STD_C_SRC), $(wildcard $(RUNTIME_DIR)/*.c))
RUNTIME_OBJ=runtime.o

# Set debug as default build
BUILD ?= debug

ifeq ($(BUILD),release)
	CARGO_FLAGS=--release
	CFLAGS=-O3 -DNDEBUG
	OUT_DIR=$(OBJ_DIR)/release
else
	CARGO_FLAGS=
	CFLAGS=-g -Og -DDEBUG
	OUT_DIR=$(OBJ_DIR)/debug
endif

# Final paths
STD_OBJS=$(addprefix $(OUT_DIR)/,$(STD_OBJ_NAMES))
RUNTIME_OBJ_PATH=$(OUT_DIR)/$(RUNTIME_OBJ)

# Compiler
CC=clang
CFLAGS+= -Wall -Wextra -std=c11 -I$(RUNTIME_DIR)

.PHONY: all debug release clean run cargo_build

# Default build
all: $(BUILD)

debug:
	@$(MAKE) BUILD=debug build

release:
	@$(MAKE) BUILD=release build

# Main build target
build: cargo_build $(STD_OBJS) $(RUNTIME_OBJ_PATH)

# Compile std_*.c into individual object files
$(OUT_DIR)/std_%.o: $(RUNTIME_DIR)/std_%.c
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Compile remaining runtime C files into runtime.o
$(RUNTIME_OBJ_PATH): $(OTHER_C_SRC)
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) -c $(OTHER_C_SRC) -o $(RUNTIME_OBJ_PATH)

# Cargo build step depends on Rust source files
cargo_build: $(RUST_SRC) Cargo.toml Cargo.lock
	cargo build $(CARGO_FLAGS)

# Run Rust binary after build
run: build
	./target/$(BUILD)/$(BINARY_NAME)

# Cleanup build artifacts
clean:
	rm -rf $(OBJ_DIR)
	cargo clean

test:
	@bash tools/run-brad-tests.sh
