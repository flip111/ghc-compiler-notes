module GHC.Compiler.Utils.Lexer
  ( Parser
  , ParseFailed(..)
  , runParser
  , runParserFromString
  , runParserFromFile
  , pTokenize
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.Conduit.Combinators  (sinkList)
import           GHC.Data.Bag              (bagToList, listToBag)
import           GHC.Data.FastString       (mkFastString)
import           GHC.Driver.Config.Parser  (initParserOpts)
import           GHC.Parser.Errors.Types   (PsMessage)
import           GHC.Parser.Lexer          (ParseResult(..), Token(..), getPsErrorMessages, lexer, unP, initParserState)
import           GHC.Types.Error           (Messages, mkMessages, getMessages)
import           GHC.Types.SrcLoc
import           Prelude
import qualified GHC.Data.StringBuffer     as StringBuffer
import qualified GHC.Driver.Session as Session
import qualified GHC.Parser.Lexer as Lexer

type Parser = Lexer.P

newtype ParseFailed = ParseFailed { pFailedMsg :: Messages PsMessage }

fromParseResult :: ParseResult a -> Either ParseFailed a
fromParseResult (POk _ x) = Right x
fromParseResult (PFailed pstate) =
  let errors    = getPsErrorMessages pstate
      errorList = bagToList (getMessages errors)
  in  Left $ ParseFailed (mkMessages (listToBag errorList))

runParser :: Parser a
          -> Session.DynFlags
          -> StringBuffer.StringBuffer
          -> RealSrcLoc
          -> Either ParseFailed a
runParser p dflags buf loc = fromParseResult (unP p $ initParserState opts buf loc)
  where opts = initParserOpts (dflags `Session.gopt_set` Session.Opt_Haddock)

runParserFromString :: Parser a -> Session.DynFlags -> String -> Either ParseFailed a
runParserFromString p dflags str = runParser p dflags buf loc
  where
    buf = StringBuffer.stringToStringBuffer str
    loc = mkRealSrcLoc (mkFastString "") 1 1

runParserFromFile :: MonadIO m => Parser a -> Session.DynFlags -> String -> m (Either ParseFailed a)
runParserFromFile p dflags fn = do
  buf <- liftIO $ StringBuffer.hGetStringBuffer fn
  pure $ runParser p dflags buf loc
  where
    loc = mkRealSrcLoc (mkFastString fn) 1 1

pTokenConsumer :: ConduitT i (Located Token) Parser ()
pTokenConsumer = go
  where
    go = do
      t <- lift $ lexer False pure
      yield t
      case unLoc t of
        ITeof -> pure ()
        _     -> go

pTokenize :: Parser [Located Token]
pTokenize = runConduit $ pTokenConsumer .| sinkList
