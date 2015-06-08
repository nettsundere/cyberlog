module Cyber.CommandParser where

import qualified Data.Text.Lazy as T

data CommandT = Install | Show | Add | Rm | Mv | Help
  deriving Show

data Arg = EntryPath { eName :: String, eVersion :: String } | VersionPath String deriving Show
data Command = Command { ct :: CommandT, arguments :: [Arg] } deriving Show

toEntryPath :: String -> Arg
toEntryPath entryPathString =
  if length split == 2
    then EntryPath eName' afterSlash
    else VersionPath eName'
  where
    split = take 2 $ map T.unpack $ T.splitOn (T.pack "/") (T.pack entryPathString)
    eName' = head split
    afterSlash = head $ tail split

parseCommand :: [String] -> Maybe Command
parseCommand [] = Nothing
parseCommand inputs = case (T.unpack . T.toLower . T.pack) (head inputs) of
  "install" -> Just $ Command Install []
  "show" -> Just $ Command Show args
  "add"  -> Just $ Command Add  args
  "rm"   -> Just $ Command Rm   args
  "mv"   -> Just $ Command Mv   args
  "help" -> Just $ Command Help args
  _ -> Nothing
  where
    args = toEntryPath <$> (tail inputs)
