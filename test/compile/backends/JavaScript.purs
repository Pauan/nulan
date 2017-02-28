module Test.Nulan.Compile.JavaScript where

import Prelude
import Data.Either (Either(..))
import Test.Unit (suite, TestSuite, failure, test)
import Test.Unit.Assert (equal)
import Nulan.Parse (parse)
import Nulan.Tokenize (tokenize)
import Nulan.Compile (compile)
import Nulan.Compile.JavaScript (generate)

testCompile :: forall a. String -> { code :: String, map :: String } -> TestSuite a
testCompile input x =
  test input
    case compile (parse (tokenize input "test.nul")) of
      Left b -> failure (show b)
      Right b -> case generate "bundle.js" "bundle.js.map" b of
        { code, map } -> do
          equal x.code code
          equal x.map map

tests :: forall a. TestSuite a
tests = suite "JavaScript" do
  testCompile "(CONSTANT foo :: Int32 1)"
    { code: "const _2 = 1;\n//# sourceMappingURL=bundle.js.map"
    , map: "{\"version\":3,\"sources\":[\"test.nul\"],\"names\":[],\"mappings\":\"AAAA,MAAU,EAAV,GAAuB,CAAvB\",\"file\":\"bundle.js\"}" }

  testCompile "(RECURSIVE (CONSTANT foo :: (FORALL a (-> a)) (-> (foo))))"
    { code: "const _2 = function () { return _2(); };\n//# sourceMappingURL=bundle.js.map"
    , map: "{\"version\":3,\"sources\":[\"test.nul\"],\"names\":[],\"mappings\":\"AAAW,MAAU,EAAV,GAAmC,cAAI,OAAC,EAAD,GAAJ,EAAnC\",\"file\":\"bundle.js\"}" }

  testCompile "(RECURSIVE (CONSTANT foo :: (FORALL a (-> a)) (-> (bar))) (CONSTANT bar :: (FORALL a (-> a)) (-> (foo))))"
    { code: "const _2 = function () { return _3(); }; const _3 = function () { return _2(); };\n//# sourceMappingURL=bundle.js.map"
    , map: "{\"version\":3,\"sources\":[\"test.nul\"],\"names\":[],\"mappings\":\"AAAW,MAAU,EAAV,GAAmC,cAAI,OAAC,EAAD,GAAJ,EAAnC,CAAX,CAA0D,MAAU,EAAV,GAAmC,cAAI,OAAC,EAAD,GAAJ,EAAnC\",\"file\":\"bundle.js\"}" }

  testCompile "(RECURSIVE (CONSTANT foo :: (-> Int32 Int32 Int32) (-> a b (foo a b))))"
    { code: "const _2 = function (_3, _4) { return _2(_3, _4); };\n//# sourceMappingURL=bundle.js.map"
    , map: "{\"version\":3,\"sources\":[\"test.nul\"],\"names\":[],\"mappings\":\"AAAW,MAAU,EAAV,GAAwC,UAAI,EAAJ,EAAM,EAAN,IAAQ,OAAC,EAAD,CAAK,EAAL,EAAO,EAAP,EAAR,EAAxC\",\"file\":\"bundle.js\"}" }

  testCompile "(CONSTANT foo :: (-> Int32 Int32 Int32) (-> a b (MATCHES [ a b ] [ 1 2 ] : 3 [ _ _ ] : 4)))"
    { code: "const _2 = function (_3, _4) { if (_3 === 1) { if (_4 === 2) { return 3; } } return 4; };\n//# sourceMappingURL=bundle.js.map"
    , map: "{\"version\":3,\"sources\":[\"test.nul\"],\"names\":[],\"mappings\":\"AAAW,MAAU,EAAV,GAAwC,UAAI,EAAJ,EAAM,EAAN,IAAQ,OAAC,EAAD,CAAK,EAAL,EAAO,EAAP,EAAR,EAAxC\",\"file\":\"bundle.js\"}" }
