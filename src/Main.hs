module Main where

import Data.Function                                    ( (&) )
import qualified Data.Text.IO as T
import Control.Concurrent                               ( tryTakeMVar )
import Control.Monad                                    ( void )
import Control.Monad.IO.Class                           ( MonadIO, liftIO )
import Control.Monad.Except                             ( runExceptT )
import Control.Monad.RWS                                ( runRWST, gets )
import System.Exit                                      ( exitWith )
import System.IO                                        ( hSetBuffering, BufferMode(LineBuffering, NoBuffering)
                                                        , stdin, stdout, stderr )
import Commands                                         ( execute, waitForCommand )
import Parser                                           ( parse )
import App                                              ( MonadApp, State(appExitCode), prepareInitialState
                                                        , defaultConfig )

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    initialState <- prepareInitialState
    -- TODO: We should catch potential exceptions in this line
    void $ runRWST mainLoop defaultConfig initialState

mainLoop :: (MonadIO m, MonadApp m) => m ()
mainLoop = do
    -- read
    liftIO $ T.putStr "> "
    line <- liftIO $ T.getLine
    -- eval-print
    result <- runExceptT $ do
        command <- parse line
        liftIO $ execute command stdin stdout stderr
    result & either (\e -> liftIO $ putStrLn ("YASH Error: " <> e)) (liftIO . waitForCommand)
    maybeExitCode <- gets appExitCode >>= (liftIO . tryTakeMVar)
    case maybeExitCode of
        Just n -> liftIO $ exitWith n
        Nothing -> mainLoop

