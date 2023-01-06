module Main (main) where

import qualified ASCII.QuasiQuoters as QQ
import qualified ASCII.TemplateHaskell as TH

import ASCII.CaseRefinement (ASCII'lower, ASCII'upper, asciiCaseUnsafe)
import ASCII.Char (Char (..))
import ASCII.Refinement (ASCII, asciiUnsafe)

import Data.ByteString (ByteString)
import Data.Function (($))
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude (Integer)
import System.IO (IO)

import qualified ASCII.Caseless as CI
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.ByteString.Lazy as LBS

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do

    describe "ASCII.QuasiQuoters" $ do

        describe "char" $ do
            it "can be ASCII.Char" $ shouldBe @Char [QQ.char|e|] SmallLetterE
            it "can be Word8" $ shouldBe @Word8 [QQ.char|e|] 101
            it "can be a pattern" $ do
                let x = case Tilde of [QQ.char|@|] -> 1; [QQ.char|~|] -> 2; _ -> 3
                shouldBe @Integer x 2

        describe "string" $ do
            it "can be [ASCII.Char]" $ shouldBe @[Char] [QQ.string|Hello!|]
                [CapitalLetterH, SmallLetterE, SmallLetterL,
                SmallLetterL, SmallLetterO, ExclamationMark]
            it "can be String" $ shouldBe @String [QQ.string|Hello!|] "Hello!"
            it "can be Text" $ shouldBe @Text [QQ.string|Hello!|] "Hello!"
            it "can be bytestring Builder" $ shouldBe @LBS.ByteString
                (BS.Builder.toLazyByteString [QQ.string|Hello!|]) "Hello!"
            it "can be a pattern" $ do
                let x = case [CapitalLetterH, SmallLetterI] of
                          [QQ.string|Bye|] -> 1; [QQ.string|Hi|] -> 2; _ -> 3
                shouldBe @Integer x 2

        describe "caseless" $ do
            it "can be [CaselessChar]" $ shouldBe [QQ.caseless|Hello!|]
                [CI.LetterH, CI.LetterE, CI.LetterL,
                CI.LetterL, CI.LetterO, CI.ExclamationMark]
            it "can be a pattern over [CaselessChar]" $ do
                let x = case [CI.LetterH, CI.LetterI] of
                          [QQ.caseless|Bye|] -> 1; [QQ.caseless|Hi|] -> 2; _ -> 3
                shouldBe @Integer x 2
            it "can be a pattern over Text" $ do
                let x = case "Hello!" :: Text of
                          [QQ.caseless|Bye!|] -> 1; [QQ.caseless|Hello!|] -> 2; _ -> 3
                shouldBe @Integer x 2
            it "matches Text in a case-insensitive manner" $ do
                let x = case "Hello!" :: Text of
                          [QQ.caseless|Bye!|] -> 1; [QQ.caseless|hEllo!|] -> 2; _ -> 3
                shouldBe @Integer x 2

        describe "upper" $ do
            it "can be Text" $ shouldBe [QQ.upper|Hello!|] ("HELLO!" :: Text)
            it "can be case-refined" $ shouldBe [QQ.upper|Hello!|]
                (asciiCaseUnsafe "HELLO!" :: ASCII'upper ByteString)
            it "can match Text" $ do
                let x = case "HI!" :: Text of
                          [QQ.upper|wow|] -> 1; [QQ.upper|Hi!|] -> 2; _ -> 3
                shouldBe @Integer x 2
            it "only matches all upper-case Text" $ do
                let x = case "Hi!" :: Text of [QQ.upper|Hi!|] -> 1; _ -> 2
                shouldBe @Integer x 2

        describe "lower" $ do
            it "can be Text" $ shouldBe [QQ.lower|Hello!|] ("hello!" :: Text)
            it "can be case-refined" $ shouldBe [QQ.lower|Hello!|]
                (asciiCaseUnsafe "hello!" :: ASCII'lower ByteString)
            it "can match Text" $ do
                let x = case "hi!" :: Text of
                          [QQ.lower|wow|] -> 1; [QQ.lower|Hi!|] -> 2; _ -> 3
                shouldBe @Integer x 2
            it "only matches all lower-case Text" $ do
                let x = case "Hi!" :: Text of [QQ.lower|Hi!|] -> 1; _ -> 2
                shouldBe @Integer x 2

    describe "ASCII.TemplateHaskell" $ do

        describe "charExp" $
            it "is a Char expression" $
                shouldBe @Char $(TH.charExp CapitalLetterF) CapitalLetterF

        describe "charPat" $ do
            it "is a Char pattern" $ do
                let x = case SmallLetterS of
                          $(TH.charPat SmallLetterR) -> 1;
                          $(TH.charPat SmallLetterS) -> 2;
                          _ -> 3
                shouldBe @Integer x 2

        describe "charListExp" $ do
            it "is a [Char] expression" $ shouldBe @[Char]
                $(TH.charListExp [CapitalLetterH, SmallLetterI])
                [CapitalLetterH, SmallLetterI]

        describe "charListPat" $ do
            it "is a [Char] pattern" $ do
                let x = case [CapitalLetterH, SmallLetterI] of
                          $(TH.charListPat [CapitalLetterH, SmallLetterA]) -> 1
                          $(TH.charListPat [CapitalLetterH, SmallLetterI]) -> 2
                          _ -> 3
                shouldBe @Integer x 2

        describe "isCharExp" $ do
            it "can be ASCII.Char expression" $
                shouldBe @Char $(TH.isCharExp CapitalLetterA) CapitalLetterA
            it "can be a Word8 expression" $
                shouldBe @Word8 $(TH.isCharExp CapitalLetterA) 65
            it "can be an ASCII Word8 expression" $
                shouldBe @(ASCII Word8)
                    $(TH.isCharExp CapitalLetterA) (asciiUnsafe 65)

        describe "isCharPat" $ do
            it "can be a Word8 pattern" $ do
                let x = case (66 :: Word8) of
                          $(TH.isCharPat CapitalLetterA) -> 1
                          $(TH.isCharPat CapitalLetterB) -> 2
                          _ -> 3
                shouldBe @Integer x 2

        describe "caselessIsStringPat" $ do
            it "matches a cased string regardless of its case" $ do
                let x = case [QQ.string|Hi!|] :: Text of
                      $(TH.caselessIsStringPat [CI.LetterG, CI.LetterO, CI.FullStop]) -> 1
                      $(TH.caselessIsStringPat [CI.LetterH, CI.LetterI, CI.ExclamationMark]) -> 2
                      _ -> 3
                shouldBe @Integer x 2
