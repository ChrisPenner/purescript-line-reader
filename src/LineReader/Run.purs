-- | This module provides an interface to read/write from a console interface using Run effects.
-- |
-- | Example usage:
-- |
-- | ```
-- | getEssay :: forall e. Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e) (Array String)
-- | getEssay = runLineReader Nothing (getWords 10)
-- |   where
-- |     getWords :: Int -> Run (aff :: AFF (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e), reader :: READER Interface) (Array String)
-- |     getWords n
-- |       | n <= 0 = pure []
-- |       | otherwise = do
-- |           newWords <- question $ ("Please enter at least " <> show n <> "words" :: String)
-- |           let splitWords = words newWords
-- |           (append splitWords <$> getWords (n - length splitWords))
-- | ```
module LineReader.Run
  ( runLineReader
  , readLine
  , question
  , module RLExports
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader (ReaderT(ReaderT))
import Data.Maybe (Maybe)
import Data.Options (Options)
import Data.Tuple (Tuple)
import LineReader (runLineReader) as LR
import Node.ReadLine.Aff (Interface, InterfaceOptions, READLINE, prompt)
import Node.ReadLine.Aff (READLINE, InterfaceOptions, createConsoleInterface, createInterface, output, completer, terminal, historySize, Completer, noCompletion) as RLExports
import Node.ReadLine.Aff (question) as RL
import Node.Stream (Readable)
import Run (AFF, Run, liftAff, runBaseAff)
import Run.Reader (READER, ask, runReader)

-- | Run a Line Reader computation from the Run Monad into the Aff Monad
runLineReader
  :: forall r a e
   . Maybe (Tuple (Readable r (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e)) (Options InterfaceOptions))
  -> Run (reader :: READER Interface, aff :: AFF (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e)) a
  -> Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e) a
runLineReader opts prog =
  LR.runLineReader opts $ ReaderT runprog
    where
      runprog :: Interface -> (Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e)) a
      runprog interface = runBaseAff (runReader interface prog)

-- | Read a single line from input.
readLine
  :: forall r e
   . Run (reader :: READER Interface
         , aff :: AFF (readline :: READLINE
                      , console :: CONSOLE
                      , exception :: EXCEPTION
                      | e)
         | r
         ) String
readLine = ask >>= prompt >>> liftAff

-- | Prompt for input, then read a line
question ::
  forall r e.
  String
  -> Run (aff :: AFF (readline :: READLINE, console :: CONSOLE | e)
         , reader :: READER Interface
         | r
         ) String
question txt = do
  interface <- ask
  liftAff (RL.question txt interface)

