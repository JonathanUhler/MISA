.PHONY: all memtest


all: memtest


%.bin: %.S common/handlers.S common/vectors.S common/memmap.ld
	@mkdir -p bin
	misa-as $< common/handlers.S common/vectors.S \
            -l="-M common/memmap.ld -g bin/$*.g"    \
            -o bin/$@


memtest: memtest.bin
