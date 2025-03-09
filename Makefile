# Compiler and flags
CC := clang
CFLAGS := -Wall -Werror -fpic

# Directories
SRC_DIR := runtime
OBJ_DIR := obj
STD_DIR := std

# Runtime source files
RUNTIME_SRC := $(filter-out $(wildcard $(SRC_DIR)/std_*.c), $(wildcard $(SRC_DIR)/*.c))
RUNTIME_OBJ := $(OBJ_DIR)/runtime.o

# Standard library source files
STD_SRC := $(wildcard $(SRC_DIR)/std_*.c)
STD_OBJ := $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR)/%.o, $(STD_SRC))

# Default target
.PHONY: all
all: runtime

# Create object directory if it doesn't exist
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Compile runtime into a single object file
$(RUNTIME_OBJ): $(RUNTIME_SRC) | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $(RUNTIME_SRC) -o $@

# Compile standard library modules
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Main targets
.PHONY: runtime release clean

runtime: $(RUNTIME_OBJ) $(STD_OBJ)
	@echo "Runtime and standard library built successfully."

release: CFLAGS += -O3 -DNDEBUG
release: runtime
	@echo "Release build completed."

debug: CFLAGS += -g -O0 -DDEBUG
debug: runtime
	@echo "Debug build completed."

clean:
	rm -rf $(OBJ_DIR)
	@echo "Build artifacts cleaned."