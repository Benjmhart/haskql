{ 
  mkDerivation,
  base,
  miso,
  http-client,
  stdenv
}:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso http-client ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
