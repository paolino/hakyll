-- | This module provides means for reading and applying 'Template's.
--
-- Templates are tools to convert data (pages) into a string. They are
-- perfectly suited for laying out your site.
--
-- Let's look at an example template:
--
-- > <html>
-- >     <head>
-- >         <title>My crazy homepage - $title$</title>
-- >     </head>
-- >     <body>
-- >         <div id="header">
-- >             <h1>My crazy homepage - $title$</h1>
-- >         </div>
-- >         <div id="content">
-- >             $body$
-- >         </div>
-- >         <div id="footer">
-- >             By reading this you agree that I now own your soul
-- >         </div>
-- >     </body>
-- > </html>
--
-- We can use this template to render a 'Page' which has a body and a @$title$@
-- metadata field.
--
-- As you can see, the format is very simple -- @$key$@ is used to render the
-- @$key$@ field from the page, everything else is literally copied. If you want
-- to literally insert @\"$key$\"@ into your page (for example, when you're
-- writing a Hakyll tutorial) you can use
--
-- > <p>
-- >     A literal $$key$$.
-- > </p>
--
-- Because of it's simplicity, these templates can be used for more than HTML:
-- you could make, for example, CSS or JS templates as well.
--
-- In addition to the native format, Hakyll also supports hamlet templates. For
-- more information on hamlet templates, please refer to:
-- <http://hackage.haskell.org/package/hamlet>.
--
module Hakyll.Web.Template
    ( Template
    , applyTemplate
    , applySelf
    , templateCompiler
    , templateCompilerWith
    , applyTemplateCompiler
    ) where

import Control.Arrow
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)
import qualified Data.Map as M

import Text.Hamlet (HamletSettings, defaultHamletSettings)

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Web.Template.Internal
import Hakyll.Web.Template.Read
import Hakyll.Web.Page.Internal

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
-- "Page". When a key is not found, it is left as it is. You can specify
-- the characters used to replace escaped dollars (@$$@) here.
--
applyTemplate :: Template -> Page String -> Page String
applyTemplate template page =
    fmap (const $ substitute =<< unTemplate template) page
  where
    map' = toMap page
    substitute (Chunk chunk) = chunk
    substitute (Key key) = fromMaybe ("$" ++ key ++ "$") $ M.lookup key map'
    substitute (Escaped) = "$"

-- | Apply a page as it's own template. This is often very useful to fill in
-- certain keys like @$root@ and @$url@.
--
applySelf :: Page String -> Page String
applySelf page = applyTemplate (readTemplate $ pageBody page) page

-- | Read a template. If the extension of the file we're compiling is
-- @.hml@ or @.hamlet@, it will be considered as a Hamlet template, and parsed
-- as such.
--
templateCompiler :: Compiler Resource Template
templateCompiler = templateCompilerWith defaultHamletSettings

-- | Version of 'templateCompiler' that enables custom settings.
--
templateCompilerWith :: HamletSettings -> Compiler Resource Template
templateCompilerWith settings =
    cached "Hakyll.Web.Template.templateCompilerWith" $
        getIdentifier &&& getResourceString >>^ uncurry read'
  where
    read' identifier string =
        if takeExtension (toFilePath identifier) `elem` [".hml", ".hamlet"]
            -- Hamlet template
            then readHamletTemplateWith settings string
            -- Hakyll template
            else readTemplate string

applyTemplateCompiler :: Identifier                            -- ^ Template
                      -> Compiler (Page String) (Page String)  -- ^ Compiler
applyTemplateCompiler identifier = require identifier (flip applyTemplate)
