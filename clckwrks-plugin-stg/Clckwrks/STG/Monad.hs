{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
module Clckwrks.STG.Monad where

import Control.Applicative           ((<$>))
import Control.Monad                 (foldM)
import Control.Monad.Reader          (MonadReader(ask,local), ReaderT(runReaderT))
import Control.Monad.State           (StateT, put, get, modify)
import Control.Monad.Trans           (MonadIO(liftIO))
import qualified Data.Text.Lazy      as LT
import Clckwrks.Acid                 (GetAcidState(..))
import Clckwrks.Monad                (Content(..), ClckT(..), ClckFormT, ClckState(..), ClckPluginsSt(..), mapClckT, runClckT, withRouteClckT, getPreProcessors)
import Clckwrks.URL                  (ClckURL)
import Clckwrks.Page.Acid            (STGState(..))
import Clckwrks.Page.Types           (Markup(..), runPreProcessors)
import Clckwrks.Page.URL             (PageURL(..), STGAdminURL(..))
import Clckwrks.Page.Types           (PageId(..))
import Clckwrks.Plugin               (clckPlugin)
import Control.Monad.Trans           (lift)
import Data.Acid                     (AcidState)
import Data.Data                     (Typeable)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Happstack.Server              (Happstack, Input, ServerPartT)
import HSP.XMLGenerator
import HSP.XML
import Text.Reform                   (CommonFormError, FormError(..))
import Web.Plugins.Core              (Plugin(..), getConfig, getPluginsSt, getPluginRouteFn)
import Web.Routes                    (RouteT(..), showURL, withRouteT)

data STGEnv = STGEnv
    { pageState        :: AcidState STGState
    , pageClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    }

type STGT m = ClckT PageURL (ReaderT STGEnv m)
type STGT' url m = ClckT url (ReaderT STGEnv m)
type PageM   = ClckT PageURL (ReaderT STGEnv (ServerPartT IO))
type STGAdminM = ClckT STGAdminURL (ReaderT STGEnv (ServerPartT IO))


runSTGT :: STGEnv -> STGT m a -> ClckT PageURL m a
runSTGT mc m = mapClckT f m
    where
      f r = runReaderT r mc

runSTGT'' :: Monad m =>
               (PageURL -> [(T.Text, Maybe T.Text)] -> T.Text)
            -> STGEnv
            -> STGT m a
            -> ClckT url m a
runSTGT'' showPageURL stripeConfig m = ClckT $ withRouteT flattenURL $ unClckT $ runSTGT stripeConfig $ m
    where
      flattenURL ::   ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> (PageURL -> [(T.Text, Maybe T.Text)] -> T.Text))
      flattenURL _ u p = showPageURL u p


-- withRouteClckT ?
flattenURLClckT :: (url1 -> [(T.Text, Maybe T.Text)] -> T.Text)
                -> ClckT url1 m a
                -> ClckT url2 m a
flattenURLClckT showClckURL m = ClckT $ withRouteT flattenURL $ unClckT m
    where
      flattenURL _ = \u p -> showClckURL u p

clckT2STGT :: (Functor m, MonadIO m, Typeable url1) =>
             ClckT url1 m a
          -> STGT m a
clckT2STGT m =
    do p <- plugins <$> get
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       flattenURLClckT clckShowFn $ mapClckT addReaderT m
    where
      addReaderT :: (Monad m) => m (a, ClckState) -> ReaderT STGEnv m (a, ClckState)
      addReaderT m =
          do (a, cs) <- lift m
             return (a, cs)

data PageFormError
    = PageCFE (CommonFormError [Input])
    | PageErrorInternal
      deriving Show

instance FormError PageFormError where
    type ErrorInputType PageFormError = [Input]
    commonFormError = PageCFE

instance (Functor m, Monad m) => EmbedAsChild (STGT m) PageFormError where
    asChild e = asChild (show e)

type PageForm = ClckFormT PageFormError PageM

instance (Monad m) => MonadReader STGEnv (STGT' url m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (STGT' url m) STGState where
    getAcidState =
        pageState <$> ask

instance (IsName n TL.Text) => EmbedAsAttr PageM (Attr n PageURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict url))

instance (IsName n TL.Text) => EmbedAsAttr PageM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- pageClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict $ showFn url []))


-- | convert 'Markup' to 'Content' that can be embedded. Generally by running the pre-processors needed.
-- markupToContent :: (Functor m, MonadIO m, Happstack m) => Markup -> ClckT url m Content
markupToContent :: (Functor m, MonadIO m, Happstack m) =>
                   Markup
                -> ClckT url m Content
markupToContent Markup{..} =
    do clckState <- get
       transformers <- getPreProcessors (plugins clckState)
       (Just clckRouteFn) <- getPluginRouteFn (plugins clckState) (pluginName clckPlugin)
       (markup', clckState') <- liftIO $ runClckT clckRouteFn clckState (foldM (\txt pp -> pp txt) (TL.fromStrict markup) transformers)
       put clckState'
       e <- liftIO $ runPreProcessors preProcessors trust (TL.toStrict markup')
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

{-
-- | update the 'currentPage' field of 'ClckState'
setCurrentPage :: (MonadIO m) => PageId -> STGT m ()
setCurrentPage pid =
    modify $ \s -> s { pageCurrent = pid }
-}
