{ stdenv, buildGoPackage, pkg-config, gtk3 }:

buildGoPackage rec {
  name = "manual-shuffle";
  src = ./.;
  goPackagePath = "github.com/orivej/manual-shuffle/golang/manual-shuffle";
  goDeps = ./deps.nix;
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ gtk3 ];
}
