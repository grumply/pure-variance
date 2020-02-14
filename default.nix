{ mkDerivation, base, deepseq, pure-json, pure-txt, unordered-containers, vector, containers, pure-txt-trie, stdenv }:
mkDerivation {
  pname = "pure-variance";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base deepseq pure-json pure-txt unordered-containers containers pure-txt-trie vector ];
  homepage = "github.com/grumply/pure-variance";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}
