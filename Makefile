.PHONY: all build-toolchain clean


all: build-toolchain


build-toolchain:
	stack build


clean:
	stack clean
