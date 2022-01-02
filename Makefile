all: build

build:
	@dune build @install

clean:
	@dune clean

test:
	$(MAKE) -C test

.PHONY: test
