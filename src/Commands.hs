module Commands
    ( Command(..)
    , execute
    , waitForCommand
    ) where

import Data.List                                ( intercalate )
import qualified Data.Text as T                
import qualified Data.Text.IO as T                
import qualified Data.Text.ICU.Regex as T
import Control.Concurrent                       ( MVar, forkIO, putMVar, newEmptyMVar, takeMVar )
import Control.Monad                            ( replicateM, void, forM_, when ) 
import Control.Monad.IO.Class                   ( liftIO )
import Control.Applicative                      ( ZipList(ZipList, getZipList) )
import System.Exit                              ( ExitCode )
import System.IO                                ( Handle )
import System.Process                           ( ProcessHandle, CreateProcess(std_in, std_out, std_err)
                                                , createProcess, createPipe, StdStream(UseHandle), proc, waitForProcess )


data Command
    = Exit (MVar ExitCode) ExitCode
    -- | Wc ...
    -- | Cat ...
    | External [T.Text]
    | Pipe [Command]
    | Grep [T.Text]
    | EmptyCommand

instance Show Command where
    show (Exit _ _) = "Exit"
    show (External parts) = T.unpack $ "External[" <> T.unwords parts <> "]"
    show (Pipe commands) = intercalate " | "  (map show commands)
    show (Grep parts) = T.unpack $ "Grep " <> T.unwords parts
    show (EmptyCommand) = "EmptyCommand"

data CommandHandle
    = RealProcess ProcessHandle
    | BuiltinCommand (MVar ())
    | EmptyHandle

instance Show CommandHandle where
    show (RealProcess _) = "<RealProcess>"
    show (BuiltinCommand _) = "<BuiltinCommand>"
    show (EmptyHandle) = "<EmptyHandle>"

waitForCommand :: CommandHandle -> IO ()
waitForCommand (RealProcess h) = void $ waitForProcess h
waitForCommand (BuiltinCommand lock) = takeMVar lock
waitForCommand EmptyHandle = pure ()

runGrep :: [T.Text] -> IO ()
runGrep [regexpText, filename] = do
    lines' <- T.lines <$> T.readFile (T.unpack filename)
    regexp' <- T.regex [] regexpText
    forM_ (zip [0 :: Int ..] lines') $ \(i, l) -> do
        T.setText regexp' l
        found <- T.findNext regexp'
        when found (putStrLn $ show i <> ": " <> T.unpack l)
runGrep _ = putStrLn "Bad arguments..."

execute :: Command -> Handle -> Handle -> Handle -> IO CommandHandle
execute (Exit var n) _ _ _ = do 
    putMVar var n
    pure EmptyHandle
execute (External parts) hStdIn hStdOut hStdErr = do
    let process = proc (T.unpack $ T.strip $ head parts) (map T.unpack $ tail parts)
        process' = process 
                   { std_in = UseHandle hStdIn
                   , std_out = UseHandle hStdOut
                   , std_err = UseHandle hStdErr
                   }
    (_, _, _, processHandle) <- createProcess process'
    pure (RealProcess processHandle)
execute (Pipe commands) hStdIn hStdOut hStdErr = do
    lock <- newEmptyMVar
    void $ forkIO $ do
        let n = length commands
        pipes <- replicateM (n - 1) createPipe
        let commands' = ZipList commands
        let inputs = ZipList (hStdIn : map fst pipes)
        let outputs = ZipList (map snd pipes ++ [hStdOut])
        let errors = ZipList (map (const hStdErr) commands)
        processHandles <- sequenceA $ getZipList (execute <$> commands' <*> inputs <*> outputs <*> errors)
        void $ mapM waitForCommand processHandles
        putMVar lock ()
    pure (BuiltinCommand lock)
execute (Grep parts) _ _ _ = do
    liftIO $ runGrep parts
    pure EmptyHandle
execute EmptyCommand _ _ _ = pure EmptyHandle

