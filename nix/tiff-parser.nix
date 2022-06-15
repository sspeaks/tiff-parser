{ mkDerivation, attoparsec, base, bytestring, hpack, hspec
, hspec-contrib, HUnit, lib, QuickCheck, hspec-attoparsec
}:
mkDerivation {
  pname = "TiffParser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base bytestring ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ attoparsec base bytestring ];
  testHaskellDepends = [
    attoparsec base bytestring hspec hspec-contrib HUnit QuickCheck hspec-attoparsec
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/TiffParser#readme";
  license = lib.licenses.bsd3;
}
