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
  ( LineReaderF
  , runLineReader
  , lineReaderToAff
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
import Node.ReadLine.Aff (question) as A
import Node.Stream (Readable)
import Run (AFF, FProxy, Run, SProxy(SProxy), interpret, interpretRec, lift, liftAff, on, runBaseAff, send)
import Run.Reader (READER, ask, runReader)

data LineReaderF r
  = ReadLine (String -> r)
  | Question String (String -> r)

derive instance functorLineReaderF :: Functor LineReaderF

type LINEREADER = FProxy LineReaderF

_linereader = SProxy :: SProxy "linereader"

handleLineReader :: forall eff r. LineReaderF ~> Run (reader :: READER Interface, aff :: AFF (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | eff) | r)
handleLineReader = case _ of
  ReadLine handleInput -> do
    interface <- ask
    response <- liftAff $ prompt interface
    pure $ handleInput response
  Question q handleInput -> do
    interface <- ask 
    response <- liftAff $ A.question q interface
    pure $ handleInput response

readLine :: forall r. Run (linereader :: LINEREADER | r) String
readLine = lift _linereader (ReadLine id)

question :: forall r. String -> Run (linereader :: LINEREADER | r) String
question q = lift _linereader (Question q id)

runLineReader
  :: forall r eff
   . Run (linereader :: LINEREADER | r)
  ~> Run (reader :: READER Interface, aff :: AFF (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | eff) | r)
runLineReader = interpret (on _linereader handleLineReader send)

-- | Run a Line Reader computation from the Run Monad into the Aff Monad
lineReaderToAff
  :: forall r a e
   . Maybe (Tuple (Readable r (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e)) (Options InterfaceOptions))
  -> Run (reader :: READER Interface, aff :: AFF (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e)) a
  -> Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e) a
lineReaderToAff opts prog =
  LR.runLineReader opts $ ReaderT runprog
    where
      runprog :: Interface -> (Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e)) a
      runprog interface = runBaseAff (runReader interface prog)
