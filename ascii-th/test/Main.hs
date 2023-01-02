module Main (main) where

import ASCII.QuasiQuoters (char, string)
import ASCII.TemplateHaskell (charExp, charListExp, charListPat, charPat,
                              isCharExp, isCharPat)

import ASCII.Char (Char (..))
import ASCII.Refinement (ASCII, asciiUnsafe)

import Data.Function (($))
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude (Integer)
import System.IO (IO)

import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.ByteString.Lazy as LBS

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do

    describe "char quasi-quoter" $ do
        it "can be ASCII.Char" $ shouldBe @Char [char|e|] SmallLetterE
        it "can be Word8" $ shouldBe @Word8 [char|e|] 101
        it "can be a pattern" $ do
            let x = case Tilde of [char|@|] -> 1; [char|~|] -> 2; _ -> 3
            shouldBe @Integer x 2

    describe "string quasi-quoter" $ do
        it "can be [ASCII.Char]" $ shouldBe @[Char] [string|Hello!|]
            [CapitalLetterH, SmallLetterE, SmallLetterL,
             SmallLetterL, SmallLetterO, ExclamationMark]
        it "can be String" $ shouldBe @String [string|Hello!|] "Hello!"
        it "can be Text" $ shouldBe @Text [string|Hello!|] "Hello!"
        it "can be bytestring Builder" $ shouldBe @LBS.ByteString
            (BS.Builder.toLazyByteString [string|Hello!|]) "Hello!"
        it "can be a pattern" $ do
            let x = case [CapitalLetterH, SmallLetterI] of
                      [string|Bye|] -> 1; [string|Hi|] -> 2; _ -> 3
            shouldBe @Integer x 2

    describe "charExp" $
        it "is a Char expression" $
            shouldBe @Char $(charExp CapitalLetterF) CapitalLetterF

    describe "charPat" $ do
        it "is a Char pattern" $ do
            let x = case SmallLetterS of $(charPat SmallLetterR) -> 1;
                                         $(charPat SmallLetterS) -> 2;
                                         _ -> 3
            shouldBe @Integer x 2

    describe "charListExp" $ do
        it "is a [Char] expression" $ shouldBe @[Char]
            $(charListExp [CapitalLetterH, SmallLetterI])
            [CapitalLetterH, SmallLetterI]

    describe "charListPat" $ do
        it "is a [Char] pattern" $ do
            let x = case [CapitalLetterH, SmallLetterI] of
                      $(charListPat [CapitalLetterH, SmallLetterA]) -> 1
                      $(charListPat [CapitalLetterH, SmallLetterI]) -> 2
                      _ -> 3
            shouldBe @Integer x 2

    describe "isCharExp" $ do
        it "can be ASCII.Char expression" $
            shouldBe @Char $(isCharExp CapitalLetterA) CapitalLetterA
        it "can be a Word8 expression" $
            shouldBe @Word8 $(isCharExp CapitalLetterA) 65
        it "can be an ASCII Word8 expression" $
            shouldBe @(ASCII Word8)
                $(isCharExp CapitalLetterA) (asciiUnsafe 65)

    describe "isCharPat" $ do
        it "can be a Word8 pattern" $ do
            let x = case (66 :: Word8) of $(isCharPat CapitalLetterA) -> 1
                                          $(isCharPat CapitalLetterB) -> 2
                                          _ -> 3
            shouldBe @Word8 x 2
