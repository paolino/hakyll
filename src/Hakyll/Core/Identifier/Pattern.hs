{-# LANGUAGE ScopedTypeVariables #-}
-- | Module providing pattern matching and capturing on 'Identifier's.
--
-- A very simple pattern could be, for example, @foo\/bar@. This pattern will
-- only match the exact @foo\/bar@ identifier.
--
-- To match more than one identifier, there are different captures that one can
-- use:
--
-- * @*@: matches exactly one element of an identifier;
--
-- * @**@: matches one or more elements of an identifier.
--
-- Some examples:
--
-- * @foo\/*@ will match @foo\/bar@ and @foo\/foo@, but not @foo\/bar\/qux@ nor
--   @foo@;
--
-- * @**@ will match any non-empty identifier;
--
-- * @foo\/**@ will match @foo\/bar@ and @foo\/bar\/qux@, but not @bar\/foo@ nor
--   @foo@;
--
-- A small warning: patterns are not globs. Using @foo\/*.markdown@ will not do
-- what you probably intended, as it will only match the file which is literally
-- called @foo\/*.markdown@. Remember that these captures only work on elements
-- of identifiers as a whole; not on parts of these elements.
--
-- Furthermore, the 'match' function allows the user to get access to the
-- elements captured by the capture elements in the pattern.
--
module Hakyll.Core.Identifier.Pattern
    ( Pattern
    , parsePattern
    , match
    , doesMatch
    , matches
    , fromCapture
    , fromCaptureString
    , fromCaptures
    , invariant

    ) where

import Data.List (intercalate, inits, tails)
import Control.Monad (msum, liftM2, forM)
import Data.Maybe (isJust)
import Data.Monoid (mempty, mappend)
import Control.Arrow ((&&&))

import Data.String (IsString, fromString)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern.Internal (Pattern (..), PatternComponent (..)) 

instance Show PatternComponent where
    show CaptureOne = "*"
    show CaptureMany = "**"
    show (Literal s) = s

instance Show Pattern where
    show = intercalate "/" . map show . unPattern

instance IsString Pattern where
    fromString = parsePattern

-- | Parse a pattern from a string
--
parsePattern :: String -> Pattern
parsePattern = Pattern . map toPattern . unIdentifier . parseIdentifier

toPattern ::  [Char] -> PatternComponent
toPattern x | x == "*"  = CaptureOne
            | x == "**" = CaptureMany
            | otherwise = Literal x

-- | Match an identifier against a pattern, generating a list of captures
--
match :: Pattern -> Identifier -> Maybe [Identifier]
match (Pattern p) (Identifier i) = fmap (map Identifier) $ match' p i

-- | Check if an identifier matches a pattern
--
doesMatch :: Pattern -> Identifier -> Bool
doesMatch p = isJust . match p

-- | Given a list of identifiers, retain only those who match the given pattern
--
matches :: Pattern -> [Identifier] -> [Identifier]
matches p = filter (doesMatch p)

-- | Split a list at every possible point, generate a list of (init, tail) cases
--
splits :: [a] -> [([a], [a])]
splits = uncurry zip . (inits &&& tails) 

-- | Internal verion of 'match'
--
match' :: [PatternComponent] -> [String] -> Maybe [[String]]
match' [] [] = Just []  -- An empty match
match' [] _ = Nothing   -- No match
match' _ [] = Nothing   -- No match
match' (m : ms) (s : ss) = case m of
    -- Take one string and one literal, fail on mismatch
    Literal l -> if s == l then match' ms ss else Nothing
    -- Take one string and one capture
    CaptureOne -> fmap ([s] :) $ match' ms ss
    -- Take one string, and one or many captures
    CaptureMany ->
        let take' (i, t) = fmap (i :) $ match' ms t
        in msum $ map take' $ splits (s : ss)

-- | Create an identifier from a pattern by filling in the captures with a given
-- string
--
-- Example:
--
-- > fromCapture (parsePattern "tags/*") (parseIdentifier "foo")
--
-- Result:
--
-- > "tags/foo"
--
fromCapture :: Pattern -> Identifier -> Identifier
fromCapture pattern = fromCaptures pattern . repeat

-- | Simplified version of 'fromCapture' which takes a 'String' instead of an
-- 'Identifier'
--
-- > fromCaptureString (parsePattern "tags/*") "foo"
--
-- Result:
--
-- > "tags/foo"
--
fromCaptureString :: Pattern -> String -> Identifier
fromCaptureString pattern = fromCapture pattern . parseIdentifier

-- | Create an identifier from a pattern by filling in the captures with the
-- given list of strings
fromCaptures ::  Pattern -> [Identifier] -> Identifier
fromCaptures p = maybe (error "Hakyll.Core.Identifier.Pattern: filling a pattern failed") id . unmatch p

unmatch :: Pattern -> [Identifier] -> Maybe Identifier
unmatch (Pattern []) _ = Just mempty
unmatch (Pattern (m : ms)) [] = case m of
    Literal l -> addCapture (Identifier [l]) $ unmatch (Pattern ms) []
    _         -> Nothing 
unmatch (Pattern (m : ms)) ids@(i : is) = case m of
    Literal l ->  addCapture (Identifier [l]) $ unmatch (Pattern ms) ids
    _         -> addCapture i $ unmatch (Pattern ms) is

addCapture :: Identifier -> Maybe Identifier -> Maybe Identifier
addCapture x = fmap (mappend x)

-- | match unmatch invariant
invariant ::  Pattern -> Identifier -> Bool
invariant p x = (match p x >>= unmatch p) == Just x

			
