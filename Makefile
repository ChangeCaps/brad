SRC_DIR      := rt/c
TEST_DIR     := rt/c/tests
OBJ_DIR      := obj
TESTS_OUT_DIR:= $(OBJ_DIR)/tests
STD_C_SRC    := $(wildcard $(SRC_DIR)/std_*.c)
STD_OBJ_NAMES:= $(patsubst $(SRC_DIR)/%.c,%.o,$(STD_C_SRC))
RUNTIME_C_SRC:= $(filter-out $(STD_C_SRC),$(wildcard $(SRC_DIR)/*.c))

# Build settings
RUNTIME_OBJ      := runtime.o
BUILD ?= debug

ifeq ($(BUILD),release)
  CFLAGS      := -O3 -DNDEBUG
  OUT_DIR     := $(OBJ_DIR)/release
else
  CFLAGS      := -DDEBUG -ggdb
  OUT_DIR     := $(OBJ_DIR)/debug
endif

# Final object paths
STD_OBJS         := $(addprefix $(OUT_DIR)/,$(STD_OBJ_NAMES))
RUNTIME_OBJ_PATH := $(OUT_DIR)/$(RUNTIME_OBJ)

# Compiler
CC           ?= clang
CFLAGS       += -Wall -Wextra -std=c23 -I$(SRC_DIR) -D_GNU_SOURCE
LDFLAGS      := -lm -lpthread -lrt -ldl -luring
TFLAGS    := -fsanitize=address,undefined -lubsan

.PHONY: all debug release build clean test run

# Default
all: $(BUILD)

debug:
	@$(MAKE) BUILD=debug build

release:
	@$(MAKE) BUILD=release build

build: $(STD_OBJS) $(RUNTIME_OBJ_PATH)

# Compile std_*.c -> individual object files
$(OUT_DIR)/std_%.o: $(SRC_DIR)/std_%.c
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Combine remaining runtime sources into one relocatable object
$(RUNTIME_OBJ_PATH): $(RUNTIME_C_SRC)
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) -r $(RUNTIME_C_SRC) -o $(RUNTIME_OBJ_PATH)

clean:
	rm -rf $(OBJ_DIR) .logs

TEST_SRCS := $(wildcard $(TEST_DIR)/*.c)
TEST_EXES := $(patsubst $(TEST_DIR)/%.c,$(TESTS_OUT_DIR)/%,$(TEST_SRCS))

test: $(TEST_EXES)
	@echo
	@echo "=== Running all tests ==="
	@for exe in $(TEST_EXES); do \
	  echo; echo ">> $$exe"; \
	  timeout 5 $$exe || exit 1; \
	  echo ">> $$exe done"; \
	done

# Alias `run` to `test`
run: test

# Compile each test file, linking in your std and runtime objects
$(TESTS_OUT_DIR)/%: $(TEST_DIR)/%.c $(STD_OBJS) $(RUNTIME_OBJ_PATH)
	@mkdir -p $(TESTS_OUT_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) $(TFLAGS) $< $(STD_OBJS) $(RUNTIME_OBJ_PATH) -o $@
