{-|

Use of these quasi-quoters in a pattern context requires the @ViewPatterns@
language extension.

-}
module ASCII.QuasiQuoters
  (
    char,
    string,
  )
  where

import ASCII.Char (Char)
import ASCII.Superset (toCharListMaybe, toCharMaybe)
import ASCII.TemplateHaskell (isCharExp, isCharPat, isStringExp, isStringPat)
import Control.Monad (return, (>=>))
import Control.Monad.Fail (MonadFail, fail)
import Data.Maybe (Maybe (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Pat, Q)

import qualified Data.Char as Unicode
import qualified Data.String as Unicode

{- | Produces an expression or a pattern corresponding to an ASCII character.

The result will have an 'ASCII.Superset.CharSuperset' constraint; since this is
polymorphic, use with a type signature to specify the particular you want is
recommended.

The quasi-quoted string must consist of a single character that is within the
ASCII character set.

In an expression context:

@
[char|e|] == SmallLetterE

[char|e|] == (101 :: Word8)
@

In a pattern context:

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
char = expPatQQ requireOneAscii isCharExp isCharPat

{- | Produces an expression or a pattern corresponding to an ASCII string.

The result will have an 'ASCII.Superset.StringSuperset' constraint; since this
is polymorphic, use with a type signature to specify the particular you want is
recommended.

The quasi-quoted string must consist only of characters are within the ASCII
character set.

In an expression context:

@
[string|Hello!|] ==
    [CapitalLetterH,SmallLetterE,SmallLetterL,SmallLetterL,SmallLetterO,ExclamationMark]

[string|Hello!|] == ("Hello!" :: 'Data.String.String')

[string|Hello!|] == ("Hello!" :: 'Data.Text.Text')

'Data.ByteString.Builder.toLazyByteString' [string|Hello!|] == "Hello!"
@

In a pattern context:

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
string = expPatQQ requireAsciiList isStringExp isStringPat

requireOneAscii :: Unicode.String -> Q Char
requireOneAscii = requireOne >=> requireAscii

oneMaybe :: [a] -> Maybe a
oneMaybe xs = case xs of [x] -> Just x; _ -> Nothing

requireOne :: Unicode.String -> Q Unicode.Char
requireOne = oneMaybe || "Must be exactly one character."

requireAscii :: Unicode.Char -> Q Char
requireAscii = toCharMaybe || "Must be an ASCII character."

requireAsciiList :: Unicode.String -> Q [Char]
requireAsciiList = toCharListMaybe || "Must be only ASCII characters."

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
