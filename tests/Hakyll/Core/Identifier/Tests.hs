{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Identifier.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2
import Data.List (group)
import Control.Monad (forM, liftM2)
import Data.Char (isAlpha)
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern.Internal
import Test.QuickCheck
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "match"
    [ Just ["bar"]            @=? match "foo/**" "foo/bar"
    , Just ["foo/bar"]        @=? match "**" "foo/bar"
    , Nothing                 @=? match "*" "foo/bar"
    , Just []                 @=? match "foo" "foo"
    , Just ["foo"]            @=? match "*/bar" "foo/bar"
    , Just ["foo/bar"]        @=? match "**/qux" "foo/bar/qux"
    , Just ["foo/bar", "qux"] @=? match "**/*" "foo/bar/qux"
    , Just ["foo", "bar/qux"] @=? match "*/**" "foo/bar/qux"
    ] ++ [testProperty "invariant match unmatch" testInvariant]


instance Arbitrary Identifier where
	arbitrary = Identifier `fmap` listOf1 (listOf1 $ arbitrary `suchThat` (liftM2 (&&) isAlpha ((/=) '/')))



genPattern :: Identifier -> Gen Pattern
genPattern (Identifier xs) = do
	ps <- forM xs $ \x -> elements [Literal x,CaptureOne,CaptureMany]
	let 	f (CaptureMany:_) = [CaptureMany]
		f ys = ys
	return $ Pattern $ concatMap f $ group ps

testInvariant = do
	x <- arbitrary
	p <- genPattern x
	return $ invariant p x

