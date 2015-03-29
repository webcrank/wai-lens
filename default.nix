{ mkDerivation, base, http-types, lens, network, stdenv, vault, wai
}:
mkDerivation {
  pname = "wai-lens";
  version = "0.1";
  src = ./.;
  buildDepends = [ base http-types lens network vault wai ];
  homepage = "https://github.com/purefn/wai-lens";
  description = "Lenses for WAI";
  license = stdenv.lib.licenses.bsd3;
}
