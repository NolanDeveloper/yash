module Parser
    ( parse
    ) where

import Data.Char                                        ( isLetter, isDigit )
import Data.Maybe                                       ( listToMaybe, fromMaybe )
import qualified Data.Text as T                         
import qualified Data.Text.Read as T                   
import qualified Data.Map as M                        
import Control.Monad                                    ( (>=>) )
import Control.Monad.Except                             ( MonadError, liftEither )
import Control.Monad.RWS                                ( gets )
import Control.Applicative                              ( (<|>) )
import qualified Data.Attoparsec.Text as At         
import System.Exit                                      ( ExitCode(ExitSuccess, ExitFailure) )
import App                                              ( MonadApp, State(appEnvironment, appExitCode) )
import Commands                                         ( Command(Exit, External, Pipe, EmptyCommand) )


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
    environment <- gets appEnvironment
    return (M.lookup name environment)

makeExitCommand :: (MonadError String m, MonadApp m) => [T.Text] -> m Command
-- TODO: if the user doesn't pass exit code it should be one of 
-- of the last executed command.
makeExitCommand args = do
    let makeExitCode firstArgument = do
          (n', _) <- liftEither (T.decimal firstArgument)
          pure (if n' == 0 then ExitSuccess else ExitFailure n')
    exitCode <- maybe (pure ExitSuccess) makeExitCode (listToMaybe args)
    var <- gets appExitCode
    pure (Exit var exitCode)

-- https://www.gnu.org/software/bash/manual/html_node/Shell-Operation.html#Shell-Operation
parse :: (MonadError String m, MonadApp m) => T.Text -> m Command
parse = breakOnParts >=> performExpansions >=> performRedirections >=> buildCommand 

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
        , Literal . T.singleton <$> At.satisfy isMetaWsOrPipe
        ])
    isMetaWsOrPipe c = not (isMeta c) && not (isWs c) && not (c `elem` ("'\"|" :: String))
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
        SingleQuoted value    -> pure value
        (Unquoted pieces)     -> T.concat <$> mapM go pieces
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
    buildSimpleCommand (cmd : args)
        -- check if cmd is builtin
        | cmd == "exit" = makeExitCommand args
        -- otherwise it's external
        | otherwise = pure $ External (cmd : args)

