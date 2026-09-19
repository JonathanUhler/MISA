# Build and run the MISA 16-bit integer ray-tracing demo.
#
# The MISA toolchain must be installed at the repository root first with `make install`. INSTALL
# points at that directory and is placed on PATH so misa-as, its helper binaries, and misa-run all
# resolve by name.

INSTALL ?= $(abspath ../../install)
export PATH := $(INSTALL):$(PATH)

SRC := raytrace.S
BIN := raytrace.bin


$(BIN): $(SRC)
	misa-as $(SRC) -o $(BIN)

run: $(BIN)
	misa-run $(BIN)

clean:
	rm -f $(BIN)

.PHONY: run clean
