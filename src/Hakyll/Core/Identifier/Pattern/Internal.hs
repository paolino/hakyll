module Hakyll.Core.Identifier.Pattern.Internal (PatternComponent (..), Pattern (..)) where

-- | One base element of a pattern
--
data PatternComponent = CaptureOne
                      | CaptureMany
                      | Literal String
                      deriving (Eq)


-- | Type that allows matching on identifiers
--
newtype Pattern = Pattern {unPattern :: [PatternComponent]}
                deriving (Eq)

