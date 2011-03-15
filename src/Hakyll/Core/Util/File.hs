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
import Data.List (isPrefixOf, (\\), nub, intersect)
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


-- | Modifications description 
data Modifications = Modifications {
	news :: [FilePath], -- ^ Files appeared
	deleted :: [FilePath], -- ^ Files disappeared
	modified :: [FilePath] -- ^ Files modified
	} deriving (Show, Eq)


-- half correct instance. It forces files which have been deleted and created to be marked as modifications. It's not correct as a delete after a create is not a modification. But correcting this bug involves mostly comparing timestamps correctly, because it can happen inside one element of the mappend.
instance Monoid Modifications where
	Modifications n d m `mappend` Modifications n' d' m' = let
		mm = nub $ m ++ m'
		nn = nub $ n ++ n'
		dd = nub $ d ++ d' 
		in Modifications ((nn \\ dd) \\ mm) ((dd \\ nn) \\ mm) (nub $ mm ++ intersect nn dd)
	mempty = Modifications [] [] []

-- | create a stateful sampling action for a hierarchy. State is needed because we compute diffs 
mkSupervisor 	:: FilePath 			-- ^ top directory
		-> IO (IO Modifications)	-- ^ null initialized stateful action
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

-- | a concurrent STM Monoid
data TMonoid m = TMonoid {
	writeTMonoid :: m -> STM (),
	readTMonoid :: STM m
	}

-- | create a TMonoid for a comparable Monoid. This is a special TMonoid which waits for an empty update to release a read
newDelayedTMonoid :: (Monoid m, Eq m) 
	=> Int			-- ^ number of empty mappends before allowing the read 
	-> STM (TMonoid m)	-- ^ a delayed TMonoid
newDelayedTMonoid n = do
	x <- newTVar mempty
	was <- newTVar 0
	let 	write y	| y == mempty = readTVar was >>= writeTVar was  . (+ 1)
			| otherwise = readTVar x >>= writeTVar x . (`mappend` y) >> writeTVar was 0
		read = do
			y <- readTVar x
			z <- readTVar was
			when (y == mempty || z < n) retry
			writeTVar x mempty
			return y
	return $ TMonoid write read

-- | track file changes in a hierarchy. This program updates the passed TMonoid. The result action kills the poller daemon 
trackPollFiles 	:: Int 			-- ^ polling delay in seconds
		-> FilePath 		-- ^ hierarchy top
		-> TMonoid Modifications 	-- ^ a monoidal STM memory cell storing last modifications
		-> IO (IO ())		-- ^ the action to kill the tracking program
trackPollFiles n top tm = do
	s <- mkSupervisor top 
	k <- forkIO . forever $ threadDelay (1000000 * n) >> s >>= atomically . writeTMonoid tm	
	return (killThread k)

-- | Execute an action on file changes in a hierarchy. 
tracker			:: Int 		-- ^ polling delay in seconds
			-> Int		-- ^ number of no-change delays before running the action 
			-> FilePath	-- ^ file hierarchy top
			-> (Modifications -> IO ()) -- ^ the action executed on non empty modifications
			-> IO (IO ())	-- ^ the action to kill the tracking program
tracker n n2 top f = do 
	tm <- atomically $ newDelayedTMonoid n2 
	kt' <- trackPollFiles n top tm
	k <- forkIO . forever $ atomically (readTMonoid tm) >>= f 
	return $ killThread k >> kt'


-- | run a 'n' seconds console reporter for the activity in hierarchy 'p'. Polling delay is 3 seconds. 
-- And it waits 2 silent polling samples before reporting
-- Medium responsiveness for an isolated change is then 3 * (2 + 1/2) seconds
testReport 	:: Int -- ^ life span for the program
		-> FilePath -- ^ hierarchy top
		-> IO () -- ^ block for life span
testReport n p = do 
	k <- tracker 3 2 p  (print . report) -- boot the tracker
	threadDelay $ n * 1000000 -- wait n seconds
	k -- kill the tracker
	where report (Modifications nn dd mm) = map length [nn,dd,mm]
