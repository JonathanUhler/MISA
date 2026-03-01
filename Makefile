.PHONY: all build-toolchain configure-sim build-sim clean


all: build-toolchain build-sim


build-toolchain:
	stack build


configure-sim:
	cd misa-sim && cmake -B build


build-sim: configure-sim
	cd misa-sim && cmake --build build


clean:
	stack clean && rm -rf misa-sim/build
