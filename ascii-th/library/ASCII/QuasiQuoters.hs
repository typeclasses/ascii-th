{-|

Use of these quasi-quoters in a pattern context requires the @ViewPatterns@
language extension.

-}
module ASCII.QuasiQuoters
  (
    {- * Character -} char,
    {- * String -} string,
    {- * Caseless string -} caseless,
    {- * Upper-case string -} upper,
    {- * Lower-case string -} lower,
  )
  where

import ASCII.Case (Case (..))
import ASCII.Caseless (CaselessChar)
import ASCII.Char (Char)
import Control.Monad (return, (>=>))
import Control.Monad.Fail (MonadFail, fail)
import Data.Maybe (Maybe (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Pat, Q)

import qualified ASCII.Superset as S
import qualified ASCII.TemplateHaskell as TH
import qualified Data.Char as Unicode
import qualified Data.String as Unicode

{-| An expression pattern corresponding to an ASCII character

=== In an expression context

The result will have a 'ASCII.Superset.FromChar' constraint.

The quasi-quoted string must consist of a single character that is within the
ASCII character set.

@
[char|e|] == SmallLetterE

[char|e|] == (101 :: Word8)
@

Since this is polymorphic, a type signature is recommended.

=== In a pattern context

The pattern matches a value of a type satisfying the 'ASCII.Superset.ToChar'
constraint.

@
let
    x = case Tilde of
          [char|@|] -> 1
          [char|~|] -> 2
          _ -> 3
in
    x == 2
@

-}

char :: QuasiQuoter
char = expPatQQ requireOneAscii TH.isCharExp TH.isCharPat

{- | An expression or pattern corresponding to an ASCII string

=== In an expression context

The result will have a 'ASCII.Superset.FromString' constraint.

The quasi-quoted string must consist only of characters are within the ASCII
character set.

@
[string|Hello!|] ==
    [CapitalLetterH,SmallLetterE,SmallLetterL,SmallLetterL,SmallLetterO,ExclamationMark]

[string|Hello!|] == ("Hello!" :: 'Data.String.String')

[string|Hello!|] == ("Hello!" :: 'Data.Text.Text')

'Data.ByteString.Builder.toLazyByteString' [string|Hello!|] == "Hello!"
@

Since this is polymorphic, a type signature is recommended.

=== In a pattern context

The pattern matches a value of a type satisfying the 'ASCII.Superset.ToString'
constraint.

@
let
    x = case [CapitalLetterH, SmallLetterI] of
          [string|Bye|] -> 1
          [string|Hi|] -> 2
          _ -> 3
in
    x == 2
@

-}

string :: QuasiQuoter
string = expPatQQ requireAsciiList TH.isStringExp TH.isStringPat

{-| An expression or pattern corresponding to a case-insensitive ASCII string

=== In an expression context

A monomorphic expression of type @['CaselessChar']@.

@
[caseless|Hello!|] ==
    [LetterH, LetterE, LetterL, LetterL, LetterO, ExclamationMark]
@

=== In a pattern context

A case-insensitive match of any type belonging to the
'ASCII.Superset.ToCaselessString' class.

@
let
    x = case "Hello!" :: 'Text' of
          [caseless|Bye!|] -> 1
          [caseless|hEllo!|] -> 2
          _ -> 3
in
    x == 2
@

-}
caseless :: QuasiQuoter
caseless = expPatQQ requireAsciiListCI TH.caselessListExp TH.caselessIsStringPat

{-| An expression or pattern corresponding to an ASCII string where all the
letters are of lower case

The letters in the body of the quasi-quotation may be written in any case
you like; they will be converted to lower case automatically.

=== In an expression context

The expression can become any type satisfying the
@('ASCII.Superset.ToCasefulString' ''LowerCase')@ constraint.
Any letters in the quoted content will be converted to lower case.

@
[lower|Hello!|] == ("hello!" :: 'Data.Text.Text')

[lower|Hello!|] == ("hello!" :: 'ASCII'lower' 'Data.ByteString.ByteString')
@

=== In a pattern context

The pattern matches a value of a type satisfying the 'ASCII.Superset.ToString'
constraint. A value matches this pattern if:

* All of the letters in the tested value are in lower case
* The tested value satisfies a case-insensitive comparison
  with the quasi-quoted content

@
let
    x = case "hi!" :: 'Text' of
          [lower|wow|] -> 1
          [lower|Hi!|] -> 2
          _ -> 3
in
    x == 2
@

-}
lower :: QuasiQuoter
lower = expPatQQ requireAsciiListCI TH.lowerStringExp
    (\x -> TH.isStringPat (S.toCasefulString @'LowerCase x))

{-| An expression or pattern corresponding to an ASCII string where all the
letters are of upper case

The letters in the body of the quasi-quotation may be written in any case
you like; they will be converted to upper case automatically.

=== In an expression context

The expression can become any type satisfying the
@('ASCII.Superset.ToCasefulString' ''UpperCase')@ constraint.
Any letters in the quoted content will be converted to upper case.

@
[upper|Hello!|] == ("HELLO!" :: 'Text')

[upper|Hello!|] == ("HELLO!" :: 'ASCII'upper' 'Data.ByteString.ByteString')
@

=== In a pattern context

The pattern matches a value of a type satisfying the 'ASCII.Superset.ToString'
constraint. A value matches this pattern if:

* All of the letters in the tested value are in upper case
* The tested value satisfies a case-insensitive comparison
  with the quasi-quoted content

@
let
    x = case "HI!" :: 'Text' of
          [QQ.upper|wow|] -> 1
          [QQ.upper|Hi!|] -> 2
          _ -> 3
in
    x == 2
@

-}
upper :: QuasiQuoter
upper = expPatQQ requireAsciiListCI TH.upperStringExp
    (\x -> TH.isStringPat (S.toCasefulString @'UpperCase x))

{-| Require the string to consist of exactly one ASCII character -}
requireOneAscii :: Unicode.String -> Q Char
requireOneAscii = requireOne >=> requireAscii

{-| Require the list to consist of exactly one element -}
oneMaybe :: [a] -> Maybe a
oneMaybe xs = case xs of [x] -> Just x; _ -> Nothing

{-| Require the string to consist of exactly one character -}
requireOne :: Unicode.String -> Q Unicode.Char
requireOne = oneMaybe || "Must be exactly one character."

{-| Require the character to be ASCII -}
requireAscii :: Unicode.Char -> Q Char
requireAscii = S.toCharMaybe || "Must be an ASCII character."

{-| Require the string to consist of all ASCII characters -}
requireAsciiList :: Unicode.String -> Q [Char]
requireAsciiList = S.toCharListMaybe || "Must be only ASCII characters."

{-| Require the string to consist of all ASCII characters,
and return them with letter case discarded -}
requireAsciiListCI :: Unicode.String -> Q [CaselessChar]
requireAsciiListCI = S.toCaselessCharListMaybe || "Must be only ASCII characters."

(||) :: (a -> Maybe b) -> Unicode.String -> a -> Q b
f || msg = \a -> case f a of Just b -> return b; Nothing -> fail msg

expPatQQ :: (Unicode.String -> Q a) -> (a -> Q Exp) -> (a -> Q Pat) -> QuasiQuoter
expPatQQ f a b = QuasiQuoter
    { quoteExp  = f >=> a
    , quotePat  = f >=> b
    , quoteType = notType
    , quoteDec  = notDec
    }

notType :: MonadFail m => a -> m b
notType _ = fail "Cannot be used in a type context."

notDec :: MonadFail m => a -> m b
notDec _ = fail "Cannot be used in a declaration context."
