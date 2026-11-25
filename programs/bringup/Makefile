.PHONY: all clean


MISA_AS ?= misa-as

EXTNS := -e dynamichw syscall privilege interrupt

PROGRAMS := firsttest memtest synctest mputest inttest
BINARIES := $(addprefix bin/, $(addsuffix .bin, $(PROGRAMS)))

COMMON  := common/vectors.S common/bringup.h common/finishtest.h


all: $(BINARIES)


bin/%.bin: %.S $(COMMON) common/memmap.ld
	@mkdir -p bin
	$(MISA_AS) $< common/vectors.S $(EXTNS)  \
            -l="-M common/memmap.ld -g bin/$*.g" \
            -o $@


clean:
	rm -rf bin
