module Main (main) where

import qualified ASCII.QuasiQuoters as QQ
import qualified ASCII.TemplateHaskell as TH

import ASCII.Char (Char (..))
import ASCII.Refinement (ASCII, asciiUnsafe)

import Data.Function (($))
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude (Integer)
import System.IO (IO)

import qualified ASCII.Caseless as Caseless
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

    describe "ASCII.TemplateHaskell" $ do

        describe "charExp" $
            it "is a Char expression" $
                shouldBe @Char $(TH.charExp CapitalLetterF) CapitalLetterF

        describe "charPat" $ do
            it "is a Char pattern" $ do
                let x = case SmallLetterS of $(TH.charPat SmallLetterR) -> 1;
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
                let x = case (66 :: Word8) of $(TH.isCharPat CapitalLetterA) -> 1
                                              $(TH.isCharPat CapitalLetterB) -> 2
                                              _ -> 3
                shouldBe @Integer x 2

        describe "caselessIsStringPat" $ do
            it "matches a cased string regardless of its case" $ do
                let x = case [QQ.string|Hi!|] :: Text of
                      $(TH.caselessIsStringPat [Caseless.LetterG, Caseless.LetterO, Caseless.FullStop]) -> 1
                      $(TH.caselessIsStringPat [Caseless.LetterH, Caseless.LetterI, Caseless.ExclamationMark]) -> 2
                      _ -> 3
                shouldBe @Integer x 2
