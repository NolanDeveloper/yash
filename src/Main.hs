module Main where

import Data.Char
import Data.Function
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import Control.Applicative
import qualified Data.Attoparsec.Text as At
import System.Environment
import System.Exit
import System.IO
import System.Process
import Debug.Trace

data Config 
    = Config {
    -- TODO: ...
    } deriving Show

data State 
    = State 
    { appIsRunning :: Bool
    , appEnvironment :: M.Map T.Text T.Text
    , appExitCode :: Int
    } deriving Show

class MonadRWS Config String State m => MonadApp m 

instance Monad m => MonadApp (RWST Config String State m)
instance MonadApp m => MonadApp (ExceptT String m)

defaultConfig :: Config
defaultConfig = Config

prepareInitialState :: IO State
prepareInitialState = do
    env <- getEnvironment
    let textEnv = map (\(a, b) -> (T.pack a, T.pack b)) env
    pure $ 
        State 
        { appIsRunning = True 
        , appEnvironment = M.fromList textEnv
        , appExitCode = 0
        }

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    initialState <- prepareInitialState
    -- TODO: We should catch exceptions in this line
    (result, finalState, log) <- runRWST mainLoop defaultConfig initialState
    let exitCode = appExitCode finalState
    liftIO $ exitWith (if exitCode == 0 then ExitSuccess else ExitFailure exitCode)
    pure result

data Command
    = Exit Int
    -- | Wc ...
    -- | Cat ...
    | External [T.Text]
    | Pipe [Command]
    | EmptyCommand
    deriving Show

makeExitCommand :: (MonadError String m, MonadApp m) => [T.Text] -> m Command
-- TODO: if the user doesn't pass exit code it should be one of 
-- of the last executed command.
makeExitCommand [] = pure (Exit 0)
makeExitCommand (exitCode : _) = do
    (exitCode', _) <- liftEither (T.decimal exitCode)
    pure (Exit exitCode')

-- TODO: We execute builtin commands immediately. It's not exactly what user expects. Fix it.
execute :: (MonadIO m, MonadError String m, MonadApp m) => Command -> Handle -> Handle -> Handle -> m (Maybe ProcessHandle)
execute (Exit n) _ _ _ = do
    modify (\s -> s { appIsRunning = False, appExitCode = n } )
    pure Nothing
execute (External parts) hStdIn hStdOut hStdErr
    | null parts = throwError "External command is empty."
    | otherwise = liftIO $ do
        let process = proc (T.unpack $ T.strip $ head parts) (map T.unpack $ tail parts)
            process' = process 
                       { std_in = UseHandle hStdIn
                       , std_out = UseHandle hStdOut
                       , std_err = UseHandle hStdErr
                       }
        (_, _, _, processHandle) <- createProcess process'
        pure $ Just processHandle
execute (Pipe commands) hStdIn hStdOut hStdErr = do
    pipes <- liftIO $ mapM (const createPipe) (tail commands)
    let inputs = hStdIn : map fst pipes
    let outputs = map snd pipes ++ [hStdOut]
    let errors = map (const hStdErr) commands
    let streamHandles = zip3 inputs outputs errors
    processHandles <- mapM (\(command, (hIn, hOut, hErr)) -> execute command hIn hOut hErr) (zip commands streamHandles)
    let maybeLast = listToMaybe . reverse . catMaybes
    return $ maybeLast processHandles

execute EmptyCommand _ _ _ = pure Nothing

data Piece
    = Literal T.Text
    | Variable T.Text
    | Escaped Char
    deriving Show

data Part 
    = Unquoted [Piece]
    | SingleQuoted T.Text
    | DoubleQuoted [Piece]
    deriving Show

lookupEnvironmentVariable :: MonadApp m => T.Text -> m (Maybe T.Text)
lookupEnvironmentVariable name = do
    env <- gets appEnvironment
    return (M.lookup name env)

-- https://www.gnu.org/software/bash/manual/html_node/Shell-Operation.html#Shell-Operation
parse :: (MonadError String m, MonadApp m) => T.Text -> m Command
parse = breakOnParts >=> performExpansions >=> performRedirections >=> buildCommand 
  where
    -- 3.1.1 Shell Operation
    --      2. Breaks the input into words and operators, obeying the quoting
    --      rules described in Quoting. These tokens are separated by
    --      metacharacters.  Alias expansion is performed by this step (see
    --      Aliases).
    --      3. Parses the tokens into simple and compound commands.
    -- Node: we don't do any expansion except from shell parameter expansion
    breakOnParts :: (MonadError String m, MonadApp m) => T.Text -> m [[Part]]
    breakOnParts = liftEither . At.parseOnly parser
      where
        parser = (part `At.sepBy` ws) `At.sepBy` (ws >> At.char '|' >> ws)

        ws = At.takeWhile1 isWs
        isWs c = c `elem` (" \t\r\n" :: String)

        part = At.choice [singleQuoted, doubleQuoted, unquoted]

        -- https://www.gnu.org/software/bash/manual/html_node/Single-Quotes.html#Single-Quotes
        singleQuoted = SingleQuoted <$> (At.char '\'' *> At.takeWhile (/= '\'') <* At.char '\'')

        -- https://www.gnu.org/software/bash/manual/html_node/Double-Quotes.html#Double-Quotes
        doubleQuoted = DoubleQuoted <$> (At.char '"' *> At.many' (At.choice 
            [ variable
            , escapedInDoubleQuotes
            , Literal . T.singleton <$> At.notChar '"'
            ]) <* At.char '"')

        variable = Variable <$> (At.char '$' *> (At.char '{' *> variableName <* At.char '}' <|> variableName))
        variableName = T.cons <$> At.satisfy isLetterOrDigit <*> At.takeWhile isLetterOrDigitOrUnderscore

        isLetterOrDigit c = isLetter c || isDigit c
        isLetterOrDigitOrUnderscore c = isLetter c || isDigit c || c == '_'

        escapedInDoubleQuotes = Escaped <$> (At.char '\\' *> At.satisfy (`elem` ("$`\"\\\n" :: String)))

        unquoted = Unquoted <$> At.many1' (At.choice 
            [ variable
            , Escaped <$> (At.char '\\' *> At.anyChar)
            , Literal . T.singleton <$> At.satisfy (\c -> not (isMeta c) && not (isWs c) && not (c `elem` ("'\"|" :: String)))
            ])
        isMeta = (`elem` ("&;()<> \t\r\n" :: String))

    -- 3.1.1 Shell Operation
    --      4. Performs the various shell expansions (see Shell Expansions),
    --      breaking the expanded tokens into lists of filenames (see Filename
    --      Expansion) and commands and arguments.
    -- Note: we only perform limited Shell parameter expansion.
    performExpansions :: MonadApp m => [[Part]] -> m [[T.Text]]
    performExpansions parts = mapM (mapM (shellParameterExpansion >=> fileNameExpansion)) parts
      where
        shellParameterExpansion :: MonadApp m => Part -> m (T.Text)
        shellParameterExpansion part = case part of
            SingleQuoted value -> pure value
            (Unquoted pieces) -> T.concat <$> mapM go pieces
            (DoubleQuoted pieces) -> T.concat <$> mapM go pieces
          where
            go :: MonadApp m => Piece -> m T.Text
            go (Literal value) = pure value
            go (Escaped value) = pure (T.singleton value)
            go (Variable name) = do
                value <- lookupEnvironmentVariable name
                pure (fromMaybe "" value)

        fileNameExpansion :: MonadApp m => T.Text -> m (T.Text)
        fileNameExpansion = pure -- TODO: expand `*` `?` `.` `..`
    
    -- 3.1.1 Shell Operation
    --      5. Performs any necessary redirections (see Redirections) and
    --      removes the redirection operators and their operands from the
    --      argument list.
    -- Note: we don't perform redirections.
    performRedirections :: MonadApp m => [[T.Text]] -> m [[T.Text]]
    performRedirections = pure

    buildCommand :: (MonadError String m, MonadApp m) => [[T.Text]] -> m Command
    buildCommand parts = Pipe <$> mapM buildSimpleCommand parts
      where
        buildSimpleCommand :: (MonadError String m, MonadApp m) => [T.Text] -> m Command
        buildSimpleCommand [] = pure $ EmptyCommand
        buildSimpleCommand [""] = pure $ EmptyCommand
        buildSimpleCommand parts@(cmd : args)
            -- check if cmd is builtin
            | cmd == "exit" = makeExitCommand args
            -- otherwise it's external
            | otherwise = pure $ External (cmd : args)

mainLoop :: (MonadIO m, MonadApp m) => m ()
mainLoop = do
    -- read
    liftIO $ T.putStr "> "
    line <- liftIO $ T.getLine
    -- eval-print
    result <- runExceptT (parse line >>= (\command -> execute command stdin stdout stderr))
    result & either (\e -> liftIO $ putStrLn ("YASH Error: " <> e)) 
                    (void . mapM (liftIO . waitForProcess))
    isRunning <- gets appIsRunning
    when isRunning mainLoop

