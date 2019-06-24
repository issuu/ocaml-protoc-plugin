.PHONY: build # Build
build:
	dune build

.PHONY: clean # Clean
clean:
	dune clean

.PHONY: test
test:
	dune runtest
