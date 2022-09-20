{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.STG.Plugin where

import Clckwrks                     ( ClckwrksConfig(clckTopDir), ClckState(plugins), ClckT(..), ClckURL, ClckPlugins, Theme
                                    , Role(..), ClckPluginsSt, addAdminMenu, addNavBarCallback, addPreProc, query, update
                                    )
import Clckwrks.Acid                (GetUACCT(..), SetUACCT(..))
import Clckwrks.Plugin              (clckPlugin)
{-
import Clckwrks.Page.Acid           (PageState, GetOldUACCT(..), ClearOldUACCT(..), initialPageState)
import Clckwrks.Page.NavBarCallback (navBarCallback)
import Clckwrks.Page.Monad          (PageConfig(..), runPageT)
import Clckwrks.Page.PreProcess     (pageCmd)
import Clckwrks.Page.Route          (routePage)
import Clckwrks.Page.URL            (PageURL(..), PageAdminURL(..))
import Clckwrks.Page.Types          (PageId(..))
-}
import Control.Applicative          ((<$>))
import Control.Monad.State          (get)
import Data.Acid                    (AcidState)
import Data.Acid.Advanced           (update', query')
import Data.Acid.Local              (createCheckpointAndClose, openLocalStateFrom,)
import Data.Text                    (Text)
import qualified Data.Text.Lazy     as TL
import Data.Maybe                   (fromMaybe)
import Data.Set                     (Set)
import qualified Data.Set           as Set
import Happstack.Server             (ServerPartT, Response, notFound, toResponse)
import System.Directory             (createDirectoryIfMissing)
import System.FilePath              ((</>))
import Web.Routes                   (toPathInfo, parseSegments, withRouteT, fromPathSegments)
import Web.Plugins.Core             (Plugin(..), Plugins(..), When(..), addCleanup, addHandler, addPostHook, initPlugin, getConfig, getPluginRouteFn)

stgHandler :: (StgURL -> [(Text, Maybe Text)] -> Text)
              -> StgConfig
              -> ClckPlugins
              -> [Text]
              -> ClckT ClckURL (ServerPartT IO) Response
stgHandler showStgURL stgConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runStgT stgConfig $ routeStg u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (StgURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showStgURL u p

stgInit :: ClckPlugins
         -> IO (Maybe Text)
stgInit plugins =
    do (Just stgShowFn) <- getPluginRouteFn plugins (pluginName stgPlugin)
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
       iss  <- initialStgState
       acid <- openLocalStateFrom (basePath </> "stg") iss
       addCleanup plugins Always (createCheckpointAndClose acid)

       let stgConfig = StgConfig { stgState     = acid
                                   , stgClckURL   = clckShowFn
                                   }

       addPreProc plugins (stgCmd acid stgShowFn)
       addNavBarCallback plugins (navBarCallback acid stgShowFn)
       addHandler plugins (pluginName stgPlugin) (stgHandler stgShowFn stgConfig)
       addPostHook plugins (migrateUACCT acid)

       return Nothing

addStgAdminMenu :: ClckT url IO ()
addStgAdminMenu =
    do p <- plugins <$> get
       (Just stgShowURL) <- getPluginRouteFn p (pluginName stgPlugin)
       let newStgURL    = stgShowURL (StgAdmin NewStg) []
           stgsURL      = stgShowURL (StgAdmin Stgs) []
           feedConfigURL = stgShowURL (StgAdmin EditFeedConfig) []
       addAdminMenu ("Stgs/Posts"
                    , [ (Set.fromList [Administrator, Editor], "New Stg/Post"   , newStgURL)
                      , (Set.fromList [Administrator, Editor], "Edit Stg/Post"  , stgsURL)
                      , (Set.fromList [Administrator, Editor], "Edit Feed Config", feedConfigURL)
                      ]
                    )

migrateUACCT :: AcidState StgState -> ClckT url IO ()
migrateUACCT acidStgState =
    do mOldUACCT <- query' acidStgState GetOldUACCT
       case mOldUACCT of
         Nothing -> return ()
         (Just uacct) ->
             do mNewUACCT <- query GetUACCT
                case mNewUACCT of
                  Nothing -> update (SetUACCT $ Just uacct)
                  (Just _) -> update' acidStgState ClearOldUACCT

stgPlugin :: Plugin StgURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
stgPlugin = Plugin
    { pluginName       = "stg"
    , pluginInit       = stgInit
    , pluginDepends    = ["clck"]
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = addStgAdminMenu
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI stgPlugin

