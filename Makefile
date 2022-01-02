all clean:
	$(MAKE) -C src $@

test:
	$(MAKE) -C test

.PHONY: test
