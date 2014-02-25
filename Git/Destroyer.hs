{- git repository destroyer
 -
 - Use with caution!
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Destroyer (
	Damage(..),
	generateDamage,
	applyDamage
) where

import Common
import Git
import Utility.QuickCheck
import Utility.FileMode

import qualified Data.ByteString as B
import Data.Word
import System.PosixCompat.Types

{- Ways to damange a git repository. -}
data Damage
	= Empty FileSelector
	| Delete FileSelector
	| Reverse FileSelector
	| AppendGarbage FileSelector B.ByteString
	| PrependGarbage FileSelector B.ByteString
	| CorruptByte FileSelector Int Word8
	| ScrambleFileMode FileSelector FileMode
	deriving (Read, Show)

instance Arbitrary Damage where
	arbitrary = oneof
		[ Empty <$> arbitrary
		, Delete <$> arbitrary
		, Reverse <$> arbitrary
		, AppendGarbage <$> arbitrary <*> garbage
		, PrependGarbage <$> arbitrary <*> garbage
		, CorruptByte
			<$> arbitrary
			<*> nonNegative arbitraryBoundedIntegral
			<*> arbitrary
		, ScrambleFileMode
			<$> arbitrary
			<*> nonNegative arbitrarySizedIntegral
		]
	  where
		garbage = B.pack <$> arbitrary `suchThat` (not . null)

{- To select a given file in a git repository, all files in the repository
 - are enumerated, sorted, and this is used as an index
 - into the list. (Wrapping around if higher than the length.) -}
data FileSelector = FileSelector Int
	deriving (Read, Show)

instance Arbitrary FileSelector where
	arbitrary = FileSelector <$> oneof
		-- An early file in the git tree, tends to be the most
		-- interesting when there are lots of files.
		[ nonNegative arbitrarySizedIntegral
		-- Totally random choice from any of the files in
		-- the git tree, to ensure good coverage.
		, nonNegative arbitraryBoundedIntegral
		]

selectFile :: [FilePath] -> FileSelector -> FilePath
selectFile sortedfs (FileSelector n) = sortedfs !! (n `mod` length sortedfs)

{- Generates random Damage. -}
generateDamage :: IO [Damage]
generateDamage = sample' (arbitrary :: Gen Damage)

{- Applies Damage to a Repo, in a reproducible fashion
 - (as long as the Repo contains the same files each time). -}
applyDamage :: [Damage] -> Repo -> IO ()
applyDamage ds r = do
	contents <- sort . filter (not . skipped . takeFileName)
		<$> dirContentsRecursive (localGitDir r)
	forM_ ds $ \d -> do
		let withfile s a = do
			let f = selectFile contents s
			-- Symlinks might be dangling, so are skipped.
			-- If the file was already removed by a previous Damage,
			-- it's skipped.
			whenM (doesFileExist f) $
				a f `catchIO` \e -> error ("Failed to apply damage " ++ show d ++ " to " ++ show f ++ ": " ++ show e ++ "(total damage: " ++ show ds ++ ")")
		case d of
			Empty s -> withfile s $ \f ->
				withSaneMode f $ do
					nukeFile f
					writeFile f ""
			Reverse s -> withfile s $ \f ->
				withSaneMode f $
					B.writeFile f =<< B.reverse <$> B.readFile f
			Delete s -> withfile s $ nukeFile
			AppendGarbage s garbage ->
				withfile s $ \f ->
					withSaneMode f $
					B.appendFile f garbage
			PrependGarbage s garbage ->
				withfile s $ \f ->
					withSaneMode f $ do
						b <- B.readFile f
						B.writeFile f $ B.concat [garbage, b]
			-- When the byte is past the end of the
			-- file, wrap around. Does nothing to empty file.
			CorruptByte s n garbage ->
				withfile s $ \f ->
					withSaneMode f $ do
						b <- B.readFile f
						let len = B.length b
						unless (len == 0) $ do
							let n' = n `mod` len
							let (prefix, rest) = B.splitAt n' b
							B.writeFile f $ B.concat
								[prefix
								, B.singleton garbage
								, B.drop 1 rest
								]
			ScrambleFileMode s mode ->
				withfile s $ \f ->
					setFileMode f mode
  where
  	-- A broken .git/config is not recoverable.
	skipped f = f `elem` [ "config" ]

withSaneMode :: FilePath -> IO () -> IO ()
withSaneMode f = withModifiedFileMode f (addModes [ownerWriteMode, ownerReadMode])
