.PHONY: build
build: ## Build
	dune build

.PHONY: clean
clean: ## Clean
	dune clean

.PHONY: install
install: dune install

.PHONY: test
test: build
test: ## Run tests
	@dune runtest --force

RUN_OCAML_PROTOC = @dune exec -- ocaml-protoc \
		-I "$(abspath $(GOOGLE_PROTOBUF_FOLDER)/../..)" \
		-int32_type int_t \
		-int64_type int_t \
		-ml_out src/spec

.PHONY: update-protobuf
update-protobuf: ## Update generated code for interfacing with protoc
	@$(eval GOOGLE_PROTOBUF_FOLDER := $(shell find /usr -type d -path '*/include/google/protobuf' 2>/dev/null | head -n 1))
	@if [ ! -d '$(GOOGLE_PROTOBUF_FOLDER)' ]; then \
		echo 'Could not find protobuf definitions for protoc plugin. Try `brew install protobuf` or `apt install libprotoc-dev`'; \
		exit 1; \
	fi
	$(RUN_OCAML_PROTOC) "$(GOOGLE_PROTOBUF_FOLDER)/compiler/plugin.proto"
	$(RUN_OCAML_PROTOC) "$(GOOGLE_PROTOBUF_FOLDER)/descriptor.proto"

%: %.proto
	protoc -I $(dir $<) $< -o/dev/stdout | protoc --decode google.protobuf.FileDescriptorSet /usr/include/google/protobuf/descriptor.proto

.PHONY: doc
doc: ## Build documentation
	dune build @doc

gh-pages: doc ## Publish documentation
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp  -r _build/default/_doc/_html/* .gh-pages
	git -C .gh-pages add .
	git -C .gh-pages config user.email 'docs@ocaml-protoc-plugin'
	git -C .gh-pages commit -m "Update documentation"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

.PHONY: help
help: ## Show this help
	@grep -h -E '^[.a-zA-Z_-]+:.*## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
