-- | Filesystem polling with an inotify backend. Works only on linux.
--
module Hakyll.Web.Preview.Poll
    ( previewPoll
    ) where

import Control.Monad (forM_, forever)
import Control.Concurrent.STM 
import Control.Concurrent 
import System.FilePath (takeDirectory)
import Data.List (nub)
import Data.Set (Set,toList)

import System.INotify (initINotify,addWatch,EventVariety (AllEvents), Event (..))

import Hakyll.Core.Configuration
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier

void f = f >> return ()
-- | Calls the given callback when the directory tree changes
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> Set Resource         -- ^ Resources to watch
            -> IO ()                -- ^ Action called when something changes or added
            -> (FilePath -> IO ())                -- ^ Action called when something is deleted
            -> IO ()                -- ^ Can block forever

previewPoll hc resources build rebuild = do
    -- Initialize inotify
    let delta = timeToWaitCreate hc
    inotify <- initINotify
    ch <- atomically $ newTChan
    tds <- atomically $ newTVar []
    let 
	decided x = atomically $ do
		ds <- readTVar tds
		writeTVar tds $ filter ((/=) x) ds
		return $ x `elem` ds 
	deleted x f = void . forkIO $ do
		atomically $ readTVar tds >>= writeTVar tds . (x:)
		threadDelay delta
		t <- decided x
		if t then f x else return ()
	
		
		
	
	
    let -- Problem: we can't add a watcher for "". So we make sure a directory
        -- name is not empty
        notEmpty "" = "."
        notEmpty x  = x

        -- A list of directories. Run it through a set so we have every
        -- directory only once.
        directories = nub . map (notEmpty . takeDirectory . toFilePath . unResource) $ toList resources

    -- Add a watcher for every directory
    forM_ directories $ \directory -> addWatch inotify [AllEvents] directory $ atomically . writeTChan ch
    forkIO . forever $ do
	e <- atomically $ readTChan ch
	case e of
		Created False p -> decided p >> build
		MovedIn False p _ -> decided p >> build
		Modified False _ -> build
		Deleted False p -> deleted p rebuild 
		MovedOut False p _ -> deleted p rebuild
		x -> return ()	
    return ()
