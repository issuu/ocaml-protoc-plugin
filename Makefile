.PHONY: build
build: ## Build
	dune build

.PHONY: clean 
clean: ## Clean
	dune clean

.PHONY: test
test: build
test: ## Run tests
	@dune runtest --force

.PHONY: format
format: ## Reformat the code
	dune build @fmt --auto-promote @install

RUN_OCAML_PROTOC = @dune exec -- ocaml-protoc \
		-I "$(abspath $(GOOGLE_PROTOBUF_FOLDER)/../..)" \
		-int32_type int_t \
		-int64_type int_t \
		-ml_out src/spec

.PHONY: update-protobuf
update-protobuf: ## Update generated code for interfacing with protoc
	@$(eval GOOGLE_PROTOBUF_FOLDER := $(shell find /usr -type d -path '*/include/google/protobuf' 2>/dev/null | head -n 1))
	@if [[ ! -d '$(GOOGLE_PROTOBUF_FOLDER)' ]]; then \
		echo 'Could not find protobuf definitions for protoc plugin. Try `brew install protobuf` or `apt install libprotoc-dev`'; \
		exit 1; \
	fi
	$(RUN_OCAML_PROTOC) "$(GOOGLE_PROTOBUF_FOLDER)/compiler/plugin.proto"
	$(RUN_OCAML_PROTOC) "$(GOOGLE_PROTOBUF_FOLDER)/descriptor.proto"

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
