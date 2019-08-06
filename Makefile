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
	protoc -I $(dir $<) --plugin=protoc-gen-ocaml=$(PLUGIN) --ocaml_out $(dir $@) $<

test: $(patsubst %.proto,%.ml,$(PROTO_FILES))
	@dune runtest

RUN_OCAML_PROTOC = @dune exec -- ocaml-protoc \
		-I "$(abspath $(GOOGLE_PROTO_FILES)/../..)" \
		-int32_type int_t \
		-int64_type int_t \
		-ml_out src/spec

.PHONY: update-protobuf
update-protobuf:
	@$(eval GOOGLE_PROTO_FILES := $(shell find /usr -type d -path '*/include/google/protobuf' 2>/dev/null | head -n 1))
	@if [[ ! -d '$(GOOGLE_PROTO_FILES)' ]]; then \
		echo 'Could not find protobuf definitions for protoc plugin. Try `brew install protobuf` or `apt install libprotoc-dev`'; \
		exit 1; \
	fi
	$(RUN_OCAML_PROTOC) "$(GOOGLE_PROTO_FILES)/compiler/plugin.proto"
	$(RUN_OCAML_PROTOC) "$(GOOGLE_PROTO_FILES)/descriptor.proto"

.PHONY: force
force:
