import Test.HUnit
import Monoparsec
import Data.Char
-- import TestInstruction

data Opt = Opt

instance Option Opt where
    emptyOpt = Opt
    warnOpt _ s = Left s

testLike1 = TestCase (assertEqual "Right like 'c'" (Right 'c') (snd $ (runParse (like 'c' :: Parsec String Opt Char) "coucou")))
-- testLike2 = TestCase (assertBool "Left like 'd'" $ shouldSimplyFail [(0, (Just (Token "c")), [Token "d"])] (snd $ runParse (like 'd') "coucou"))

testLike = TestList [testLike1]

testOneOf1 = TestCase (assertEqual "Right oneOf" (Right 'c') (snd $ runParse (oneOf "abc" :: Parsec String Opt Char) "coucou"))
-- testOneOf2 = TestCase (assertBool "Left oneOf" $ shouldSimplyFail [(0, (Just (Token "c")), [Token "dxy"])] (snd $ runParse (oneOf "dxy") "coucou la famille"))

testOneOf = TestList [testOneOf1]

testNoneOf1 = TestCase (assertEqual "Right noneOf" (Right 'c') (snd $ runParse (noneOf "de" :: Parsec String Opt Char ) "coucou"))
-- testNoneOf2 = TestCase (assertBool "Left noneOf" $ shouldSimplyFail [(0, Just (Token "c"), [Token ""])] (snd $ runParse (noneOf "cxy") "coucou"))

testNoneOf = TestList [testNoneOf1]

testInteger1 = TestCase (assertEqual "Right integer" (Right 123) (snd $ runParse (integer :: Parsec String Opt Int) "123"))
testInteger2 = TestCase (assertEqual "Right negative integer" (Right (-456)) (snd $ runParse (integer :: Parsec String Opt Int) "-456"))
-- testInteger3 = TestCase (assertBool "Left invalid integer" $ shouldSimplyFail [(0, (Just (Token "-")), [Token "1a"])] (snd $ runParse integer "-1a"))

testInteger = TestList [testInteger1, testInteger2]

testAny1 = TestCase (assertEqual "Right any" (Right 'c') (snd $ runParse (Monoparsec.any :: Parsec String Opt Char) "coucou"))
-- testAny2 = TestCase (assertBool "Left any" $ shouldSimplyFail [(0, (Just (Token "c")), [Token "d"])] (snd $ runParse (Monoparsec.any :: Parsec String Char) "coucou"))

testAny = TestList [testAny1]

testString1 = TestCase (assertEqual "Right string" (Right "abc") (snd $ runParse (string "abc" :: Parsec String Opt String ) "abcdef"))
-- testString2 = TestCase (assertBool "Left string" $ shouldSimplyFail [(0, (Just (Token "abc")), [Token "def"])] (snd $ runParse (string "def") "abcdef"))

testString = TestList [testString]

testSomeUntil1 = TestCase (assertEqual "Right someUntil" (Right "cou") (snd $ runParse (someUntil Monoparsec.any (like 'c' :: Parsec String Opt Char)) "coucou"))
-- testSomeUntil2 = TestCase (assertBool "Left someUntil" $ shouldSimplyFail [(2, (Just (Token "ou")), [Token "c"])] (snd $ runParse (someUntil Monoparsec.any (like 'c')) "coucou"))

testSomeUntil = TestList [testSomeUntil1]

testManyUntil1 = TestCase (assertEqual "Right manyUntil" (Right "co") (snd $ runParse (manyUntil Monoparsec.any (like 'u' :: Parsec String Opt Char)) "coucou"))
-- testManyUntil2 = TestCase (assertBool "Left manyUntil" $ shouldSimplyFail [(2, (Just (Token "ou")), [Token "c"])] (snd $ runParse (manyUntil Monoparsec.any (like 'c')) "coucou"))

testManyUntil = TestList [testManyUntil1]

testSatisfy1 = TestCase (assertEqual "Right satisfy isDigit" (Right '5') (snd $ runParse (satisfy Data.Char.isDigit :: Parsec String Opt Char) "5"))
-- testSatisfy2 = TestCase (assertBool "Left satisfy isDigit" $ shouldSimplyFail [(0, (Just (Token ['5'])), [Token ['6']])] (snd $ runParse (satisfy Data.Char.isDigit) "6"))

testSatisfy = TestList [testSatisfy1]

testWhitespace1 = TestCase (assertEqual "Right whitespace" (Right ()) (snd $ runParse (whitespace :: Parsec String Opt ()) "   abc"))
-- testWhitespace2 = TestCase (assertBool "Left whitespace" $ shouldSimplyFail [(0, (Just (Token "c")), [Token "oucou la famille"])] (snd $ runParse whitespace "coucou la famille"))

testWhitespace = TestList [testWhitespace1]

testsfunc = TestList [testLike, testOneOf, testNoneOf, testInteger, testAny, testSomeUntil, testManyUntil, testSatisfy, testWhitespace]

main :: IO Counts
main = runTestTT testsfunc
