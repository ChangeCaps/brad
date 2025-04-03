RUNTIME_DIR=rt/c
OBJ_DIR=obj
STD_C_SRC=$(wildcard $(RUNTIME_DIR)/std_*.c)
STD_OBJ_NAMES=$(patsubst $(RUNTIME_DIR)/%.c,%.o,$(STD_C_SRC))
OTHER_C_SRC=$(filter-out $(STD_C_SRC), $(wildcard $(RUNTIME_DIR)/*.c))
RUNTIME_OBJ=runtime.o
BUILD ?= debug

ifeq ($(BUILD),release)
	CFLAGS=-O3 -DNDEBUG
	OUT_DIR=$(OBJ_DIR)/release
else
	CFLAGS=-g -Og -DDEBUG
	OUT_DIR=$(OBJ_DIR)/debug
endif

# Final paths
STD_OBJS=$(addprefix $(OUT_DIR)/,$(STD_OBJ_NAMES))
RUNTIME_OBJ_PATH=$(OUT_DIR)/$(RUNTIME_OBJ)

# Compiler
CC=clang
CFLAGS+= -Wall -Wextra -std=c11 -I$(RUNTIME_DIR)

.PHONY: all debug release clean run

# Default build
all: $(BUILD)

debug:
	@$(MAKE) BUILD=debug build

release:
	@$(MAKE) BUILD=release build

build: $(STD_OBJS) $(RUNTIME_OBJ_PATH)

# Compile std_*.c into individual object files
$(OUT_DIR)/std_%.o: $(RUNTIME_DIR)/std_%.c
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) -c $< -o $@
# Compile remaining runtime C files into runtime.o
$(RUNTIME_OBJ_PATH): $(OTHER_C_SRC)
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) -c $(OTHER_C_SRC) -o $(RUNTIME_OBJ_PATH)
# Cleanup build artifacts
clean:
	rm -rf $(OBJ_DIR) .logs
test:
	@bash tools/run-brad-tests.sh
