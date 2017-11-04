{ mkDerivation, aeson, base, bytestring, hspec, network-uri
, QuickCheck, quickcheck-instances, stdenv, test-invariant, text
}:
mkDerivation {
  pname = "siren";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base network-uri text ];
  testHaskellDepends = [
    aeson base bytestring hspec network-uri QuickCheck
    quickcheck-instances test-invariant text
  ];
  homepage = "https://github.com/alunduil/siren";
  description = "Siren Tools for Haskell";
  license = stdenv.lib.licenses.mit;
}
