{
  description = "My parity env";

  inputs = {
    honggfuzz = {
      url = "github:rust-fuzz/honggfuzz-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, honggfuzz }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.default = pkgs.mkShell {
        name = "honggfuzz-shell";

        packages = with pkgs; [
          libbfd
          bintools-unwrapped
          libunwind
          lldb

          honggfuzz.packages.${system}.default
        ];

        # Fortify causes build failures: 'str*' defined both normally and as 'alias' attribute
        hardeningDisable = [ "fortify" ];
      };
    }
  );
}
