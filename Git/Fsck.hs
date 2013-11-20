{- git fsck interface
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Fsck (
	FsckResults,
	MissingObjects,
	findBroken,
	foundBroken,
	findMissing,
) where

import Common
import Git
import Git.Command
import Git.Sha
import Git.CatFile
import Utility.Batch

import qualified Data.Set as S
import System.Process (std_err, std_out)
import Control.Concurrent

type MissingObjects = S.Set Sha

{- If fsck succeeded, Just a set of missing objects it found.
 - If it failed, Nothing. -}
type FsckResults = Maybe MissingObjects

{- Runs fsck to find some of the broken objects in the repository.
 - May not find all broken objects, if fsck fails on bad data in some of
 - the broken objects it does find.
 -
 - Strategy: Rather than parsing fsck's current specific output,
 - look for anything in its output (both stdout and stderr) that appears
 - to be a git sha. Not all such shas are of broken objects, so ask git
 - to try to cat the object, and see if it fails.
 -
 - Some forms of corruption will crash fsck in ways that does not let it
 - output the sha of the broken object. In such a case, Nothing will be
 - returned. In this case, to find broken objects, re-run with tryharder
 - set to True. This makes fsck run in verbose mode, so it prints out
 - shas before checking them. We assume that the last sha is the one that
 - it crashed on, and it may have crashed following from eg, a commit to a
 - tree to a subtree. So, run git show on the sha, and examin the stderr
 - to find an actual bad sha.
 -}
findBroken :: Bool -> Bool -> Repo -> IO FsckResults
findBroken batchmode tryharder r = do
	(output, fsckok) <- processTranscript command' (toCommand params') Nothing
	let objs = findShas output
	if fsckok || not tryharder
		then do
			badobjs <- findMissing objs r
			if S.null badobjs && not fsckok
				then return Nothing
				else return $ Just badobjs
		else case lastMaybe objs of
			Nothing -> return Nothing
			Just o -> do
				p@(_, _, _, pid) <- createProcess $
					( proc "git" $ toCommand $ gitCommandLine [ Param "show", Param $ show o ] r )
					{ std_err = CreatePipe
					, std_out = CreatePipe
					}
				void $ forkIO $ void $ hGetContents (stdoutHandle p)
				objs' <- findShas <$>
					hGetContentsStrict (stderrHandle p)
				badobjs <- findMissing objs' r
				ifM (checkSuccessProcess pid)
					( if S.null badobjs
						then return Nothing
						else return $ Just badobjs
					, return $ Just $ S.singleton o
					)

  where
	(command, params) = ("git", fsckParams tryharder r)
	(command', params')
		| batchmode = toBatchCommand (command, params)
		| otherwise = (command, params)

foundBroken :: FsckResults -> Bool
foundBroken Nothing = True
foundBroken (Just s) = not (S.null s)

{- Finds objects that are missing from the git repsitory, or are corrupt.
 -
 - Note that catting a corrupt object will cause cat-file to crash;
 - this is detected and it's restarted.
 -}
findMissing :: [Sha] -> Repo -> IO MissingObjects
findMissing objs r = go objs [] =<< start
  where
	start = catFileStart' False r
	go [] c h = do
		void $ tryIO $ catFileStop h
		return $ S.fromList c
	go (o:os) c h = do
		v <- tryIO $ isNothing <$> catObjectDetails h o
		case v of
			Left _ -> do
				void $ tryIO $ catFileStop h
				go os (o:c) =<< start
			Right True -> go os (o:c) h
			Right False -> go os c h

findShas :: String -> [Sha]
findShas = catMaybes . map extractSha . concat . map words . lines

fsckParams :: Bool -> Repo -> [CommandParam]
fsckParams verbose = gitCommandLine $
	[ Param "fsck"
	, Param "--no-dangling"
	, Param "--no-reflogs"
	] ++ if verbose then [ Param "--verbose" ] else []
