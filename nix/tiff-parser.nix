{ mkDerivation, attoparsec, base, binary, bytestring, hspec
, hspec-attoparsec, hspec-contrib, HUnit, lib, QuickCheck
}:
mkDerivation {
  pname = "TiffParser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base binary bytestring ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    attoparsec base binary bytestring hspec hspec-attoparsec
    hspec-contrib HUnit QuickCheck
  ];
  homepage = "https://github.com/githubuser/TiffParser#readme";
  license = lib.licenses.bsd3;
}
