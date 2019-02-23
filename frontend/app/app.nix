{ 
  mkDerivation,
  base,
  miso,
  classy-prelude,
  http-conduit,
  aeson,
  bytestring,
  yaml,
  text,
  http-client,
  stdenv
}:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso http-client http-conduit aeson ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
