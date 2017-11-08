{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, hspec, http-media, http-types, network-uri
, QuickCheck, quickcheck-instances, stdenv, test-invariant, text
, unordered-containers
}:
mkDerivation {
  pname = "siren";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers http-media http-types network-uri
    text unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive containers hspec http-media
    http-types network-uri QuickCheck quickcheck-instances
    test-invariant text unordered-containers
  ];
  homepage = "https://github.com/alunduil/siren";
  description = "Siren Tools for Haskell";
  license = stdenv.lib.licenses.mit;
}
