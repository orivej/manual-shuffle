{ stdenv, buildGoModule, pkg-config, gtk3 }:

buildGoModule rec {
  name = "manual-shuffle";
  src = ./.;
  vendorHash = "sha256-zrkIW5OPjnqHVn0y0zr5KOU9lAysIXXsJtV3D2jiKh4=";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ gtk3 ];
}
