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
data Damage = Damage DamageAction FileSelector
	deriving (Read, Show)

instance Arbitrary Damage where
	arbitrary = Damage <$> arbitrary <*> arbitrary

data DamageAction
	= Empty
	| Delete
	| Reverse
	| AppendGarbage B.ByteString
	| PrependGarbage B.ByteString
	| CorruptByte Int Word8
	| ScrambleFileMode FileMode
	deriving (Read, Show)

instance Arbitrary DamageAction where
	arbitrary = oneof
		[ pure Empty
		, pure Delete
		, pure Reverse
		, AppendGarbage <$> garbage
		, PrependGarbage <$> garbage
		, CorruptByte
			<$> nonNegative arbitraryBoundedIntegral
			<*> arbitrary
		, ScrambleFileMode <$> nonNegative arbitrarySizedIntegral
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

{- Generates random Damage.
 -
 - TODO: sample' only seems to go up to 20 for files? -}
generateDamage :: IO [Damage]
generateDamage = sample' (arbitrary :: Gen Damage)

{- Applies Damage to a Repo, in a reproducible fashion
 - (as long as the Repo contains the same files each time). -}
applyDamage :: [Damage] -> Repo -> IO ()
applyDamage l r = do
	contents <- sort . filter (not . skipped . takeFileName)
		<$> dirContentsRecursive (localGitDir r)
	forM_ l $ \(Damage action fileselector) -> do
		let f = selectFile contents fileselector
		-- Symlinks might be dangling, so are skipped.
		-- If the file was already removed by a previous Damage,
		-- it's skipped.
		whenM (doesFileExist f) $
			applyDamageAction action f
				`catchIO` \e -> error ("Failed to apply " ++ show action ++ " " ++ show f ++ ": " ++ show e ++ "(total damage: " ++ show l ++ ")")
  where
  	-- A broken .git/config is not recoverable.
	skipped f = f `elem` [ "config" ]

applyDamageAction :: DamageAction -> FilePath -> IO ()
applyDamageAction Empty f = withSaneMode f $ do
	nukeFile f
	writeFile f ""
applyDamageAction Reverse f = withSaneMode f $
	B.writeFile f =<< B.reverse <$> B.readFile f
applyDamageAction Delete f = nukeFile f
applyDamageAction (AppendGarbage garbage) f = withSaneMode f $
	B.appendFile f garbage
applyDamageAction (PrependGarbage garbage) f = withSaneMode f $ do
	b <- B.readFile f
	B.writeFile f $ B.concat [garbage, b]
-- When the byte is past the end of the file, wrap around.
-- Does nothing to empty file.
applyDamageAction (CorruptByte n garbage) f = withSaneMode f $ do
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
applyDamageAction (ScrambleFileMode mode) f = setFileMode f mode

withSaneMode :: FilePath -> IO () -> IO ()
withSaneMode f = withModifiedFileMode f (addModes [ownerWriteMode, ownerReadMode])
