BUILD_DIR   ?= build
INSTALL_DIR ?= install


.PHONY: all build build-toolchain install clean


all: build install


build: build-toolchain


build-toolchain:
	stack build --work-dir $(BUILD_DIR)


CLI_SCRIPTS := $(wildcard cli/*.py)
CLI_TARGETS := $(patsubst cli/%.py, $(INSTALL_DIR)/%, $(CLI_SCRIPTS))


install: $(CLI_TARGETS)
	@mkdir -p $(INSTALL_DIR)
	stack install --work-dir $(BUILD_DIR) --local-bin-path $(INSTALL_DIR)


$(INSTALL_DIR)/%: cli/%.py
	cp $< $@
	chmod +x $@


clean:
	stack clean
