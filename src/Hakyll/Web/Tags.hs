-- | Module containing some specialized functions to deal with tags.
-- This Module follows certain conventions. My advice is to stick with them if
-- possible.
--
-- More concrete: all functions in this module assume that the tags are
-- located in the @tags@ field, and separated by commas. An example file
-- @foo.markdown@ could look like:
--
-- > ---
-- > author: Philip K. Dick
-- > title: Do androids dream of electric sheep?
-- > tags: future, science fiction, humanoid
-- > ---
-- > The novel is set in a post-apocalyptic near future, where the Earth and
-- > its populations have been damaged greatly by Nuclear...
--
-- All the following functions would work with such a format. In addition to
-- tags, Hakyll also supports categories. The convention when using categories
-- is to place pages in subdirectories.
--
-- An example, the page @posts\/coding\/2010-01-28-hakyll-categories.markdown@
-- Tags or categories are read using the @readTags@ and @readCategory@
-- functions. This module only provides functions to work with tags:
-- categories are represented as tags. This is perfectly possible: categories
-- only have an additional restriction that a page can only have one category
-- (instead of multiple tags).
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, Arrows #-}
module Hakyll.Web.Tags
    ( Tags (..)
    , readTagsWith
    , readTags
    , readCategory
    , renderTagCloud
    , renderTagsField
    , renderCategoryField
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (intersperse)
import Control.Arrow (arr, (&&&), (>>>), (***), (<<^), returnA)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mconcat)

import Data.Typeable (Typeable)
import Data.Binary (Binary, get, put)
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Util.Url
import Hakyll.Core.Writable
import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Core.Util.String

-- | Data about tags
--
data Tags a = Tags
    { tagsMap :: Map String [Page a]
    } deriving (Show, Typeable)

instance Binary a => Binary (Tags a) where
    get = Tags <$> get
    put (Tags m) = put m

instance Writable (Tags a) where
    write _ _ = return ()

-- | Obtain tags from a page
--
getTags :: Page a -> [String]
getTags = map trim . splitAll "," . getField "tags"

-- | Obtain categories from a page
--
getCategory :: Page a -> [String]
getCategory = return . getField "category"

-- | Higher-level function to read tags
--
readTagsWith :: (Page a -> [String])  -- ^ Function extracting tags from a page
             -> [Page a]              -- ^ Pages
             -> Tags a                -- ^ Resulting tags
readTagsWith f pages = Tags
    { tagsMap = foldl (M.unionWith (++)) M.empty (map readTagsWith' pages)
    }
  where
    -- Create a tag map for one page
    readTagsWith' page =
        let tags = f page
        in M.fromList $ zip tags $ repeat [page]

-- | Read a tagmap using the @tags@ metadata field
--
readTags :: [Page a] -> Tags a
readTags = readTagsWith getTags

-- | Read a tagmap using the @category@ metadata field
--
readCategory :: [Page a] -> Tags a
readCategory = readTagsWith getCategory

-- | Render a tag cloud in HTML
--
renderTagCloud :: (String -> Identifier)    -- ^ Produce a link for a tag
               -> Double                    -- ^ Smallest font size, in percent
               -> Double                    -- ^ Biggest font size, in percent
               -> Compiler (Tags a) String  -- ^ Tag cloud renderer
renderTagCloud makeUrl minSize maxSize = proc (Tags tags) -> do
    -- In tags' we create a list: [((tag, route), count)]
    tags' <- mapCompiler ((id &&& (getRouteFor <<^ makeUrl)) *** arr length)
                -< M.toList tags

    let -- Absolute frequencies of the pages
        freqs = map snd tags'

        -- Find out the relative count of a tag: on a scale from 0 to 1
        relative count = (fromIntegral count - min') / (1 + max' - min')

        -- Show the relative size of one 'count' in percent
        size count =
            let size' = floor $ minSize + relative count * (maxSize - minSize)
            in show (size' :: Int) ++ "%"

        -- The minimum and maximum count found, as doubles
        (min', max')
            | null freqs = (0, 1)
            | otherwise = (minimum &&& maximum) $ map fromIntegral freqs

        -- Create a link for one item
        makeLink ((tag, url), count) =
            H.a ! A.style (toValue $ "font-size: " ++ size count)
                ! A.href (toValue $ fromMaybe "/" $ toUrl `fmap` url)
                $ toHtml tag

    -- Render and return the HTML
    returnA -< renderHtml $ mconcat $ intersperse " " $ map makeLink tags'

-- | Render tags with links
--
renderTagsFieldWith :: (Page a -> [String])        -- ^ Function to get the tags
                    -> String                      -- ^ Destination key
                    -> (String -> Identifier)      -- ^ Create a link for a tag
                    -> Compiler (Page a) (Page a)  -- ^ Resulting compiler
renderTagsFieldWith tags destination makeUrl =
    id &&& arr tags >>> setFieldA destination renderTags
  where
    -- Compiler creating a comma-separated HTML string for a list of tags
    renderTags :: Compiler [String] String
    renderTags =   arr (map $ id &&& makeUrl)
               >>> mapCompiler (id *** getRouteFor)
               >>> arr (map $ uncurry renderLink)
               >>> arr (renderHtml . mconcat . intersperse ", " . catMaybes)

    -- Render one tag link
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

-- | Render tags with links
--
renderTagsField :: String                      -- ^ Destination key
                -> (String -> Identifier)      -- ^ Create a link for a tag
                -> Compiler (Page a) (Page a)  -- ^ Resulting compiler
renderTagsField = renderTagsFieldWith getTags

-- | Render the category in a link
--
renderCategoryField :: String                      -- ^ Destination key
                    -> (String -> Identifier)      -- ^ Create a category link
                    -> Compiler (Page a) (Page a)  -- ^ Resulting compiler
renderCategoryField = renderTagsFieldWith getCategory
