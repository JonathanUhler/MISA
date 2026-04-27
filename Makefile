BUILD_DIR   ?= build
INSTALL_DIR ?= install


.PHONY: all build build-toolchain docs install clean


all: build install


build: build-toolchain


build-toolchain:
	stack build --work-dir $(BUILD_DIR)


docs:
	cd docs/arch && quarto render --to pdf


CLI_SCRIPTS := $(wildcard cli/misa-*.py)
CLI_TARGETS := $(patsubst cli/misa-%.py, $(INSTALL_DIR)/misa-%, $(CLI_SCRIPTS))


install: $(CLI_TARGETS)
	@mkdir -p $(INSTALL_DIR)
	cp cli/helpers.py $(INSTALL_DIR)
	stack install --work-dir $(BUILD_DIR) --local-bin-path $(INSTALL_DIR)


$(INSTALL_DIR)/%: cli/%.py
	@mkdir -p $(INSTALL_DIR)
	cp $< $@
	chmod +x $@


clean:
	rm -rf $(BUILD_DIR) $(INSTALL_DIR)
