{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment
import System.Directory
import System.FilePath
import Cyber.CommandParser
import Control.Monad
import Data.List

usageMessage :: String
usageMessage = "Usage: [COMMAND] [arguments..] where COMMAND is: \n \
  \cyberlog install \
  \ \n Prepare this directory to use Cyberlog \n\n \
  \cyberlog add  \"version\" \
  \ \n Add new version \n\n \
  \cyberlog add  \"version/entry-name\" \
  \ \n Add new entry \n\n \
  \cyberlog mv   \"version/entry-name\" \"other-version/other-entry-name\"  \
  \ \n Move entry between versions \n\n \
  \cyberlog rm   \"version/entry-name\" \
  \ \n Remove an entry from the version \n\n \
  \cyberlog rm   \"version\"\
  \ \n Remove entire version from TANGELOG \n\n \
  \cyberlog show \"version/entry-name\" \
  \ \n Show single entry \n\n \
  \cyberlog show \"version\" \
  \ \n Show version entries \n\n \
  \cyberlog show \
  \ \n Show entire TANGELOG \n\n \
  \cyberlog help \
  \ \n Show this HELP \n"

cyberlogSubDirectory :: FilePath
cyberlogSubDirectory = "_cyberlog"

relativeToAbsolute :: FilePath -> IO FilePath
relativeToAbsolute relativePath =
  getCurrentDirectory >>= \currentDirectory ->
    return $ currentDirectory </> cyberlogSubDirectory </> relativePath

initCyberlog :: IO ()
initCyberlog =
  getCurrentDirectory >>= \currentDirectory ->
    createDirectory (currentDirectory </> cyberlogSubDirectory)

sortedFilesInDirectory :: FilePath -> IO [FilePath]
sortedFilesInDirectory path =
  getDirectoryContents path >>= \contents ->
    return (sort $ filter (\x -> x /= "." && x /= "..") contents)

getCurrentVersions :: IO [FilePath]
getCurrentVersions =
  relativeToAbsolute "" >>= \path ->
    sortedFilesInDirectory path >>= \files ->
      filterM (\x -> doesDirectoryExist (path </> x)) (reverse files)

getEntries :: String -> IO [FilePath]
getEntries version =
  relativeToAbsolute version >>= \path ->
    sortedFilesInDirectory path >>= \files ->
      filterM (\x -> doesFileExist (path </> x)) files

getEntryContents :: FilePath -> FilePath -> IO T.Text
getEntryContents version entry =
  relativeToAbsolute version >>= \path ->
    T.readFile $ path </> entry

getVersionContents :: String -> IO T.Text
getVersionContents version =
  getEntries version >>= \entriesList ->
    mapM (getEntryContents version) entriesList >>= \versionsContents ->
      return $ T.unlines [ versionHeader, "", T.concat versionsContents ]
        where versionHeader = T.pack ("## " ++ version ++ " ##")

addVersion :: String -> IO ()
addVersion version = relativeToAbsolute version >>= createDirectory

addEntry :: String -> String -> IO ()
addEntry version entry = relativeToAbsolute (version </> entry) >>= \path ->
  writeFile path ""

rmVersion :: String -> IO()
rmVersion version = relativeToAbsolute version >>= removeDirectoryRecursive

rmEntry :: String -> String -> IO ()
rmEntry ver ent =
  relativeToAbsolute (ver </> ent) >>= removeFile

mvVersion :: String -> String -> IO ()
mvVersion ver ver' =
  relativeToAbsolute "" >>= \path ->
    renameDirectory (path </> ver) (path </> ver')

mvEntry :: String -> String -> String -> String -> IO ()
mvEntry ver ent ver' ent' =
  relativeToAbsolute "" >>= \cyberRoot ->
    renameFile (cyberRoot </> ver </> ent) (cyberRoot </> ver' </> ent')

perform :: Command -> IO ()

perform (Command Help _) = putStrLn usageMessage

perform (Command Install _) = initCyberlog

perform (Command Show []) =
  getCurrentVersions >>= \versions ->
    (mapM getVersionContents versions) >>= \versionsContents ->
      T.putStrLn $ T.concat $ versionsContents

perform (Command Show [VersionPath ver]) =
  getVersionContents ver >>= T.putStrLn

perform (Command Show [EntryPath ver ent]) =
  getEntryContents ver ent >>= T.putStrLn

perform (Command Add [VersionPath ver]) =
  addVersion ver

perform (Command Add [EntryPath ver ent]) =
  addEntry ver ent

perform (Command Rm [VersionPath ver]) =
  rmVersion ver

perform (Command Rm [EntryPath ver ent]) =
  rmEntry ver ent

perform (Command Mv (VersionPath ver : [ VersionPath ver' ])) =
  mvVersion ver ver'

perform (Command Mv (EntryPath ver ent : [ EntryPath ver' ent' ])) =
  mvEntry ver ent ver' ent'

perform (Command Show _) = error "Bad show arguments"
perform (Command Add _)  = error "Bad add arguments"
perform (Command Rm _)   = error "Bad remove arguments"
perform (Command Mv  _)  = error "Bad move arguments"

main :: IO ()
main = do
  response <- (parseCommand <$> getArgs)
  case response of
    Just cmd -> perform cmd
    Nothing  -> putStrLn usageMessage
