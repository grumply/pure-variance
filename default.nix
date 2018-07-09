{ mkDerivation, base, deepseq, pure-json, pure-txt, unordered-containers, vector, stdenv }:
mkDerivation {
  pname = "pure-variance";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base deepseq pure-json pure-txt unordered-containers vector ];
  homepage = "github.com/grumply/pure-variance";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}