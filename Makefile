.PHONY: build # Build
build:
	dune build

.PHONY: clean # Clean
clean:
	dune clean

.PHONY: test
PROTO_FILES = $(wildcard src/test/*.proto)
PLUGIN=_build/default/src/plugin/ocaml_protoc_plugin.exe
$(PLUGIN): force
	dune build src/plugin/ocaml_protoc_plugin.exe


src/test/%.ml: src/test/%.proto $(PLUGIN)
	protoc -I /usr/include/ -I $(dir $<) --plugin=protoc-gen-ocaml=$(PLUGIN) --ocaml_out $(dir $@) $<

test: $(patsubst %.proto,%.ml,$(PROTO_FILES))
	@dune runtest

.PHONY: force
force:
