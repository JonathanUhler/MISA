.PHONY: all configure-sim build-sim clean


all: build-toolchain build-sim


build-toolchain:
	stack build


build:
	cd misa-sim && cmake -B build


build-sim: build
	cd misa-sim && cmake --build build


clean:
	stack clean && rm -rf misa-sim/build
