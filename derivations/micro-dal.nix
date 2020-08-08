{ mkDerivation, base, base58-bytestring, bytestring, conduit
, containers, cryptonite, exceptions, fetchgit, filepath, hspec
, hspec-discover, hspec-expectations, hspec-need-env, http-client
, http-client-tls, interpolatedstring-perl6, memory, minio-hs
, QuickCheck, safe, sqlite-simple, stdenv, store
, string-conversions, text, unliftio
}:
mkDerivation {
  pname = "micro-dal";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ivanovs-4/micro-dal";
    sha256 = "0w5p1yqifyrfsc59rqcsh32zd16k2cybr5g7p5j231aj5acjmh2b";
    rev = "d172b9f0a1586026d83da2742ff47bdab4d4d5d9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base58-bytestring bytestring conduit cryptonite exceptions
    filepath http-client http-client-tls interpolatedstring-perl6
    memory minio-hs safe sqlite-simple store string-conversions text
    unliftio
  ];
  testHaskellDepends = [
    base containers hspec hspec-discover hspec-expectations
    hspec-need-env QuickCheck store string-conversions text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/hexresearch/micro-dal#readme.md";
  description = "Lightweight Data Access Layer";
  license = stdenv.lib.licenses.bsd3;
}
