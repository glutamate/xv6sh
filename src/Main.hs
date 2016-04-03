module Main where

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Directory
import System.Process
import System.Environment
import Control.Exception  hiding (handle)
import Control.Monad
import System.IO
import GHC.IO.Exception
import Data.List

data ShSt = ShSt { interactive :: Bool }

type ShM = StateT ShSt IO

main :: IO ()
main = do
  args <- getArgs
  let (beh, st) = case args of
        [] -> (defaultBehavior, ShSt True)
        (fnm:_) -> (useFile fnm, ShSt False)
  flip evalStateT st $ runInputTBehavior beh defaultSettings runShell

chomp :: String -> String
chomp = reverse . chomp' . reverse . chomp' where
  chomp' (' ':s) = chomp' s
  chomp' s = s

runShell :: InputT ShM ()
runShell = do
  interact <- haveTerminalUI
  let prompt = if interact then "$ " else ""
  mln <- handle (\Interrupt -> return $ Just [] ) $ withInterrupt $ getInputLine prompt
  case fmap processLn mln of
    Just ("exit":_) -> return ()
    Just [] -> runShell
    Nothing -> return ()
    Just ln -> lift (goLine ln) >> runShell

processLn :: String -> [String]
processLn = words . takeWhile (/='#') . chomp

goLine, cd :: [String] -> ShM ()

goLine ("cd":rest) = cd rest
goLine s = case parseCmd s of
  Nothing -> return ()
  Just cmd -> liftIO $ execCmd cmd

cd [] = do
  home <- liftIO $ getHomeDirectory
  cd [home]
cd (dir:_) = do
  res <- liftIO $ try $ setCurrentDirectory dir
  case res of
    Right () -> return ()
    Left e -> do
      liftIO $ putErr $ dir ++ ": "++ioe_description (e :: IOException)
      return ()

execCmd :: Cmd -> IO ()
execCmd = go Inherit [] [] where
  go inStream procHans openStreams cmd = do
    mb_fpcmd <- findExecutable $ cmd_call cmd
    case mb_fpcmd of
      Nothing -> do putErr $ cmd_call cmd ++ ": command not found"
                    waitForProcesses procHans
                    closeStreams openStreams
      Just fpcmd -> do
        stdIn <- case cmd_in cmd of
          Nothing -> return inStream
          Just fp -> fmap UseHandle $ openFile fp ReadMode
        stdOut <- case cmd_out cmd of
          Just fp -> fmap UseHandle $ openFile fp WriteMode
          Nothing -> case cmd_pipeto cmd of
            Nothing -> return Inherit
            Just _ -> return CreatePipe
        let cP = (proc fpcmd (cmd_args cmd)) {std_out = stdOut, std_in = stdIn }
        (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess cP
        case cmd_pipeto cmd of
          Nothing -> do waitForProcesses (ph:procHans)
                        closeStreams (stdIn:stdOut:openStreams)
          Just cmdTo -> go (maybe Inherit UseHandle mb_stdout_hdl)
                           (ph:procHans)
                           (stdIn:stdOut:openStreams)
                           cmdTo



closeStreams :: [StdStream] -> IO ()
closeStreams =  mapM_ closeStream . reverse where
  closeStream (UseHandle h) = hClose h
  closeStream _ = return ()

waitForProcesses :: [ProcessHandle] -> IO ()
waitForProcesses = mapM_ (void . waitForProcess) . reverse

putErr  = hPutStrLn stderr

data Cmd = Cmd
  { cmd_call :: String
  , cmd_args :: [String]
  , cmd_in :: Maybe String
  , cmd_out :: Maybe String
  , cmd_pipeto :: Maybe Cmd
  }

parseCmd :: [String] -> Maybe Cmd
parseCmd [] = Nothing
parseCmd (cmd0:rest) = parseArgs (Cmd cmd0 [] Nothing Nothing Nothing) rest where
  parseArgs cmd [] = Just cmd
  parseArgs cmd ("|":rest) = do
    to <- parseCmd rest
    Just $ cmd {cmd_pipeto = Just to}
  parseArgs cmd (">":fnm:rest) = parseArgs (cmd {cmd_out = Just fnm}) rest
  parseArgs cmd ("<":fnm:rest) = parseArgs (cmd {cmd_in = Just fnm}) rest
  parseArgs cmd (arg:rest) = parseArgs (cmd {cmd_args = cmd_args cmd ++ [arg] }) rest



{- TODO

set variables

-}
