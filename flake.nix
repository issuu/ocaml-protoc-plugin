{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let package = "ocaml-protoc-plugin";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        devPackages = {
            ocaml-lsp-server = "*";
          };
        scope = on.buildOpamProject {
            resolveArgs = {
              dev = true;
              with-test=true;
            };
          } package ./. devPackages;
        overlay = final: prev: {
            ${package} = prev.${package}.overrideAttrs (super: {
              doNixSupport = false;
            });
          };
      in {
        legacyPackages = scope.overrideScope' overlay;

        packages.default = self.legacyPackages.${system}.${package};

        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.legacyPackages.${system}.${package} ];
          buildInputs = builtins.map
              (s: builtins.getAttr s self.legacyPackages.${system})
              (builtins.attrNames devPackages);
        };
      });
}
