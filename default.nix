{ mkDerivation, aeson, base, bytestring, containers, hspec
, hspec-discover, http-media, http-types, network-arbitrary
, network-uri, network-uri-json, QuickCheck, quickcheck-instances
, stdenv, test-invariant, text, unordered-containers
}:
mkDerivation {
  pname = "siren-json";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers http-media http-types network-uri
    network-uri-json text unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers hspec http-media http-types
    network-arbitrary network-uri network-uri-json QuickCheck
    quickcheck-instances test-invariant text unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/siren-json.hs";
  description = "Siren Tools for Haskell";
  license = stdenv.lib.licenses.mit;
}
