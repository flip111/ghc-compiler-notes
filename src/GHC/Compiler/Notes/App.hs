{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Compiler.Notes.App where

import           Capability.Accessors
import           Capability.Reader

import           Control.Monad
import           Control.Monad.Catch

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT(..))

import           Control.Monad.Trans.State

import           Data.Kind

import qualified Data.Text                  as Text

import qualified GHC.Driver.Flags as Flags
import qualified GHC.Driver.Session as Session
import qualified GHC.Driver.Monad as GHCMonad

import           GHC                        (getSessionDynFlags, runGhc)

import           GHC.Compiler.Notes.Types
import           GHC.Generics

import           GHC.LanguageExtensions     (Extension)
import qualified GHC.Paths

import GHC.Types.SrcLoc

data AppContext = AppContext { envSession :: Session.DynFlags
                             , envSourceResourceGetter :: FilePath -> Maybe SrcSpan -> Text.Text
                             }
  deriving Generic

type OnOff a = (Bool, a)

data AppContextCommon = AppContextCommon { sFlagsExtensions         :: [OnOff Extension]
                                         , sFlagsGeneralOptions     :: [OnOff Flags.GeneralFlag]
                                         , sFlagsGlobalIncludePaths :: [String]
                                         }

ghcInitSession :: MonadIO m => m Session.DynFlags
ghcInitSession = liftIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags


appContext :: MonadIO m => AppContextCommon -> m AppContext
appContext AppContextCommon{..} = do
  defDflags <- ghcInitSession
  let dflags = execState dflagsUpdater defDflags

  return $ AppContext { envSession = dflags, envSourceResourceGetter = gitlabResourceGetter }
  where
    dflagsUpdater = do
      forM_ sFlagsGeneralOptions $ \(b, opt) -> modify'
        (\dflags -> if b then dflags `Session.gopt_set` opt else dflags `Session.gopt_unset` opt)
      forM_ sFlagsExtensions $ \(b, ext) -> modify'
        (\dflags -> if b then dflags `Session.xopt_set` ext else dflags `Session.xopt_unset` ext)
      modify' (\dflags ->
        dflags { Session.includePaths = Session.addGlobalInclude (Session.includePaths dflags)
                                                                   sFlagsGlobalIncludePaths
               })

    gitlabResourceGetter fn opts =
      "https://gitlab.haskell.org/ghc/ghc/tree/master/" <> Text.pack fn <> case opts of
        Nothing -> ""
        Just s  -> case srcSpanStart s of
          RealSrcLoc l _ -> "#L" <> Text.pack (show $ srcLocLine l)
          _ -> "#"

defaultAppContext :: MonadIO m => m AppContext
defaultAppContext = appContext $
  AppContextCommon { sFlagsExtensions         = []
                   , sFlagsGeneralOptions     =
                       [(True, Flags.Opt_Haddock), (True, Flags.Opt_KeepRawTokenStream)]
                   , sFlagsGlobalIncludePaths = [ "dummy_includes"
                                                , "output/ghc/compiler"
                                                , "output/ghc/libraries/base/include"
                                                ]
                   }

newtype AppT (m :: Type -> Type) a = AppT { runAppT :: AppContext -> m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    ) via (ReaderT AppContext m)

deriving
  via (MonadReader (ReaderT AppContext m))
  instance Monad m => HasReader "AppContext" AppContext (AppT m)

deriving
  via (Field "envSession" "ctx" (MonadReader (ReaderT AppContext m)))
  instance Monad m => HasReader "envSession" Session.DynFlags (AppT m)

instance Monad m => HasSourceResourceGetter (AppT m) where
  sourceResourceGetter fn opts = do
    ctx <- ask @"AppContext"
    pure $ envSourceResourceGetter ctx fn opts
