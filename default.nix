{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, hspec, http-media, http-types, network-uri
, network-uri-json, QuickCheck, quickcheck-instances, stdenv
, test-invariant, text, unordered-containers
}:
mkDerivation {
  pname = "siren-json";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers http-media http-types network-uri
    network-uri-json text unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive containers hspec http-media
    http-types network-uri network-uri-json QuickCheck
    quickcheck-instances test-invariant text unordered-containers
  ];
  homepage = "https://github.com/alunduil/siren-json.hs";
  description = "Siren Tools for Haskell";
  license = stdenv.lib.licenses.mit;
}
