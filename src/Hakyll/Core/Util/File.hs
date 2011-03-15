{-# LANGUAGE TupleSections #-}
-- | A module containing various file utility functions
--
module Hakyll.Core.Util.File
    ( makeDirectories
     , getRecursiveContents
    ) where

import Control.Applicative ((<$>))
import Data.Monoid (Monoid (..), mempty, mappend)
import Control.Monad.List (ListT (ListT), runListT,guard, when, forever)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM ( retry, newTVar, readTVar, writeTVar, atomically, STM)
import Control.Monad.Trans (lift)
import Data.List (isPrefixOf, (\\), nub)
import Data.Maybe (catMaybes)
import System.Directory ( getModificationTime, createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath ( normalise, takeDirectory, (</>))
import System.Time (ClockTime)


-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
--
makeDirectories :: FilePath -> IO ()
makeDirectories = createDirectoryIfMissing True . takeDirectory

-- | Get all contents of a directory. Note that files starting with a dot (.)
-- will be ignored.
--
getRecursiveContents :: FilePath       -- ^ Directory to search
                     -> IO [(FilePath, ClockTime)]  -- ^ List of files found
getRecursiveContents = runListT . getRecursiveContents' where 	
	getRecursiveContents' path = do
	 	pathIsDir <- lift (doesDirectoryExist path) 
		if pathIsDir then do 
			name <- ListT $ getDirectoryContents path
			guard . not . isPrefixOf "." $ name
			getRecursiveContents' . normalise $ path </> name
		else (path,) <$> lift (getModificationTime path)

data Modifications = Modifications {
	news :: [FilePath],
	deleted :: [FilePath],
	modified :: [FilePath]
	} deriving (Show, Eq)

instance Monoid Modifications where
	Modifications n d m `mappend` Modifications n' d' m' = Modifications (nub $ n ++ n') (nub $ d ++ d') (nub $ m ++ m')
	mempty = Modifications [] [] []

mkSupervisor :: FilePath -> IO (IO Modifications)
mkSupervisor top = let
	news' ws xs = map fst xs \\ map fst ws 
	deleteds' ws xs = map fst ws \\ map fst xs
	modified' ws xs = catMaybes $ do 
		(x,t) <- xs
		return $  lookup x ws >>= \t' -> if t /= t' then Just x else Nothing
	in do 	t <- atomically $ newTVar []
		return $ do 
			xs <- getRecursiveContents top 
			ws <- atomically $ do 
				ws <- readTVar t
				writeTVar t xs
				return ws
			return (Modifications (news' ws xs) (deleteds' ws xs) (modified' ws xs))

data TMonoid m = TMonoid {
	writeTMonoid :: m -> STM (),
	readTMonoid :: STM m
	}

newTMonoid = do
	x <- newTVar mempty
	let 	write y = readTVar x >>= writeTVar x . flip mappend y
		read = do
			y <- readTVar x
			when (y == mempty) retry
			writeTVar x mempty
			return y
	return $ TMonoid write read

trackPollFiles :: Int -> FilePath -> IO (IO Modifications, IO ())
trackPollFiles n top = do
	t <- atomically newTMonoid 
	s <- mkSupervisor top 
	k <- forkIO . forever $ threadDelay n >> s >>= atomically . writeTMonoid t	
	return (atomically $ readTMonoid t, killThread k)

trackFPollFiles :: Int -> FilePath -> (Modifications -> IO ()) -> IO (IO ())	
trackFPollFiles n top f = do 
	(s,kt') <- trackPollFiles n top
	k <- forkIO . forever $ s >>= f 
	return (killThread k >> kt')
