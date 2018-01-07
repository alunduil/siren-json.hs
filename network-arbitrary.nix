{ mkDerivation, base, bytestring, case-insensitive, fetchgit, hspec
, http-media, http-types, network-uri, QuickCheck, stdenv
, test-invariant
}:
mkDerivation {
  pname = "network-arbitrary";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/alunduil/network-arbitrary";
    sha256 = "033203lhkhqbb2kfjm2z6pbfp6hc277ks192brs6w81a97d6609v";
    rev = "f3bcc1960422b708262ccb7273b89dde45dd67fd";
  };
  libraryHaskellDepends = [
    base bytestring http-media http-types network-uri QuickCheck
  ];
  testHaskellDepends = [
    base bytestring case-insensitive hspec http-media http-types
    network-uri QuickCheck test-invariant
  ];
  homepage = "https://github.com/alunduil/network-arbitrary";
  description = "Arbitrary Instances for Network Types";
  license = stdenv.lib.licenses.mit;
}
