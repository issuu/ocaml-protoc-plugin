.PHONY: build
build: ## Build
	@dune build

.PHONY: clean
clean: ## Clean
	@dune clean

.PHONY: test
test: build
test: ## Run tests
	@dune runtest --force

.PHONY: install
install: build ## Install
	@dune install

.PHONY: uninstall
uninstall: build ## uninstall
	@dune uninstall

RUN_OCAML_PROTOC = @dune exec -- ocaml-protoc \
		-I "$(abspath $(GOOGLE_PROTOBUF_FOLDER)/../..)" \
		-int32_type int_t \
		-int64_type int_t \
		-ml_out src/spec

%: %.proto
	protoc -I $(dir $<) $< -o/dev/stdout | protoc --decode google.protobuf.FileDescriptorSet /usr/include/google/protobuf/descriptor.proto

.PHONY: google
# We actually dont want this. If we do, then if the user want derivers, its not possible.
# We could make an alternative package, which depends on common derivers, and adds these.
# That would actually be much better.
google: PROTO_FILES = $(wildcard /usr/include/google/protobuf/*proto) $(wildcard /usr/include/google/protobuf/compiler/*proto)
google: build
	protoc -I /usr/include/ --plugin=protoc-gen-ocaml=_build/default/src/plugin/protoc_gen_ocaml.exe "--ocaml_out=annot=[@@deriving show { with_path = false }, eq]:src/google/." $(PROTO_FILES)


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
