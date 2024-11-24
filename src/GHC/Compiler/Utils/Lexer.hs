module GHC.Compiler.Utils.Lexer where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Data.Conduit
import           Data.Conduit.Combinators  (sinkList)

import qualified GHC.Driver.Flags as Flags
import qualified GHC.Driver.Session as Session

import           GHC.Data.FastString       (mkFastString)

import qualified GHC.Parser.Lexer as Lexer
import           GHC.Parser.Lexer          (ParseResult(..), Token(..), getPsErrorMessages)

import qualified GHC.Utils.Outputable as Outputable

import GHC.Utils.Outputable (ppr)

import           GHC.Utils.Ppr as Pretty                 (Doc)

import GHC.Types.SrcLoc

import qualified GHC.Data.StringBuffer as StringBuffer
import GHC.Driver.Config.Parser (initParserOpts)
import qualified GHC.Types.Error
import GHC.Types.Error (MsgEnvelope, Messages, errMsgDiagnostic)
import qualified GHC.Parser.Errors.Types
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Data.Bag (bagToList, listToBag)

type Parser = Lexer.P

newtype ParseFailed = ParseFailed { pFailedMsg ::Messages PsMessage }

fromParseResult :: ParseResult a -> Either ParseFailed a
fromParseResult (POk _ x)        = Right x
fromParseResult (PFailed pstate) =
  let errors = getPsErrorMessages pstate
      errorList = bagToList (GHC.Types.Error.getMessages errors)
  in  Left $ ParseFailed (GHC.Types.Error.mkMessages (listToBag errorList))

-- ???????????????????????????
-- fromParseResult :: ParseResult a -> Either ParseFailed a
-- fromParseResult (PR pr) =
--   case unpackPR pr of
--     Left _pstate ->
--       let errors = getPsErrorMessages _pstate
--           errorList = bagToList (GHC.Types.Error.getMessages errors)
--       in Left $ ParseFailed (GHC.Types.Error.mkMessages (listToBag errorList))
--     Right result -> Right result
--   where
--     unpackPR :: (# (# PState, a #) | PState #) -> Either PState a
--     unpackPR (# (# _pstate, result #) | #) = Right result
--     unpackPR (# | _pstate #) = Left _pstate
-- ???????????????????????????

runParser :: Parser a
          -> Session.DynFlags
          -> StringBuffer.StringBuffer
          -> RealSrcLoc
          -> Either ParseFailed a
runParser p dflags buf loc = fromParseResult $ Lexer.unP p $ Lexer.initParserState opts buf loc
  where opts   = initParserOpts (dflags `Session.gopt_set` Session.Opt_Haddock)

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
