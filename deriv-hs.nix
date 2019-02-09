{ mkDerivation, base, bytestring, stdenv }:
mkDerivation {
  pname = "deriv-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring ];
  license = stdenv.lib.licenses.bsd3;
}
