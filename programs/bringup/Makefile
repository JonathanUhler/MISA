.PHONY: all firsttest memtest


all: firsttest memtest


%.bin: %.S common/handlers.S common/vectors.S common/memmap.ld
	@mkdir -p bin
	misa-as $< common/handlers.S common/vectors.S \
            -l="-M common/memmap.ld -g bin/$*.g"    \
            -o bin/$@


firsttest: firsttest.bin


memtest: memtest.bin
