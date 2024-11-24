module GHC.Compiler.Utils.Lexer where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Data.Conduit
import           Data.Conduit.Combinators  (sinkList)

import qualified GHC.Driver.Flags as Flags
import qualified GHC.Driver.Session as Session

import           GHC.Data.FastString       (mkFastString)

import qualified GHC.Parser.Lexer as Lexer
import           GHC.Parser.Lexer          (ParseResult(..), Token(..))

import qualified GHC.Utils.Outputable as Outputable

import           GHC.Utils.Ppr as Pretty                 (Doc)

import GHC.Types.SrcLoc

import qualified GHC.Data.StringBuffer as StringBuffer
import GHC.Driver.Config.Parser (initParserOpts)
import qualified GHC.Types.Error
import qualified GHC.Parser.Errors.Types

type Parser = Lexer.P

newtype ParseFailed = ParseFailed { pFailedMsg :: GHC.Types.Error.Messages GHC.Parser.Errors.Types.PsMessage }

fromParseResult :: Session.DynFlags -> ParseResult a -> Either ParseFailed a
fromParseResult _ (POk _ x) = Right x
fromParseResult dflags (PFailed _ s d) = Left $ ParseFailed [(s, Outputable.runSDoc d ctx)]
  where
    ctx = Session.initSDocContext dflags $ Outputable.defaultErrStyle dflags


runParser :: Parser a
          -> Session.DynFlags
          -> StringBuffer.StringBuffer
          -> RealSrcLoc
          -> Either ParseFailed a
runParser p dflags buf loc = fromPR $ Lexer.unP p $ Lexer.initParserState opts buf loc
  where
    opts   = initParserOpts (dflags `Session.gopt_set` Session.Opt_Haddock)
    fromPR = fromParseResult dflags

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
      t <- lift $ Lexer.lexer False pure
      yield t
      case unLoc t of
        ITeof -> pure ()
        _     -> go

pTokenize :: Parser [Located Token]
pTokenize = runConduit $ pTokenConsumer .| sinkList
