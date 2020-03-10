module App 
    ( Config(..), State(..)
    , defaultConfig, prepareInitialState
    , MonadApp
    ) where

import qualified Data.Text as T
import qualified Data.Map as M      
import Control.Concurrent                                   ( MVar, newEmptyMVar )
import Control.Monad.Except                                 ( ExceptT )
import Control.Monad.RWS                                    ( RWST, MonadRWS )
import System.Environment                                   ( getEnvironment )
import System.Exit                                          ( ExitCode )


data Config 
    = Config {
    -- TODO: ...
    } deriving Show

data State 
    = State 
    { appEnvironment :: M.Map T.Text T.Text
    , appExitCode :: MVar ExitCode
    }

class MonadRWS Config String State m => MonadApp m 

instance Monad m => MonadApp (RWST Config String State m)
instance MonadApp m => MonadApp (ExceptT String m)

defaultConfig :: Config
defaultConfig = Config

prepareInitialState :: IO State
prepareInitialState = do
    environment <- map (\(a, b) -> (T.pack a, T.pack b)) <$> getEnvironment
    exitCodeMVar <- newEmptyMVar
    pure $ 
        State 
        { appEnvironment = M.fromList environment
        , appExitCode = exitCodeMVar
        }
