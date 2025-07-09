{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "i686-linux" ] (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        defaultPackage = pkgs.mkShell {
          name = "lispoggers";
          buildInputs = with pkgs; [ stack ghc
                                     hlint entr
                                     zlib ];

          shellHook = "stack --version && ghc --version";
        };
      }
    );
}
