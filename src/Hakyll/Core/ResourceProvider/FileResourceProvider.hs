-- | A concrete 'ResourceProvider' that gets it's resources from the filesystem
--
module Hakyll.Core.ResourceProvider.FileResourceProvider
    ( fileResourceProvider
    ) where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as LB

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File

-- | Create a filesystem-based 'ResourceProvider'
--
fileResourceProvider :: (FilePath -> Bool) -> IO ResourceProvider
fileResourceProvider ignoreFile = do
    -- Retrieve a list of identifiers
    list <- map parseIdentifier . filter (not . ignoreFile) <$>
        getRecursiveContents False "."

    -- Construct a resource provider
    return ResourceProvider
        { resourceList           = map Resource list
        , resourceString         = readFile . toFilePath . unResource
        , resourceLazyByteString = LB.readFile . toFilePath . unResource
        }
