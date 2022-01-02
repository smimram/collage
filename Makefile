all: build

build:
	@dune build

test:
	$(MAKE) -C test

.PHONY: test
