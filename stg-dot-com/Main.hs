{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}
module Main where

import Clckwrks             (ClckT, ClckURL, ClckPlugins, ClckwrksConfig(..), ClckState, TLSSettings(..), plugins)
import Clckwrks.GetOpts     (parseArgs, clckwrksOpts)
import Clckwrks.Server      (simpleClckwrks)
import Clckwrks.Plugin      (clckPlugin)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Page.Plugin (pagePlugin)
import Clckwrks.Media.Plugin (mediaPlugin)
-- import Clckwrks.MailingList.Plugin (mailingListPlugin)
import Control.Monad        (msum)
import Data.Text            (Text, unpack)
import Happstack.Server     (Request(rqPaths), Response, ServerPartT, Browsing(DisableBrowsing), nullDir, seeOther, toResponse, localRq, serveDirectory)
import Web.Plugins.Core     (addHandler, initPlugin, setTheme)
import System.Environment   (getArgs)
-- we use 'PackageImports' because the 'Theme' module is supplied by multiple packages
-- import "clckwrks-theme-bootstrap" Theme (theme)
import Theme (theme)

------------------------------------------------------------------------------
-- ClckwrksConfig
------------------------------------------------------------------------------

-- | default configuration. Most of these options can be overridden on
-- the command-line accept for 'clckInitHook'.
clckwrksConfig :: ClckwrksConfig
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"  -- hostname of the server
    , clckHidePort        = False        -- should the port number be used in generated URLs
    , clckPort            = 8000         -- port to listen on
    , clckTLS             = Nothing      -- TLS settings
    , clckJQueryPath      = "js/jquery"  -- directory containing 'jquery.js'
    , clckJQueryUIPath    = ""           -- directory containing 'jquery.js'
    , clckJSTreePath      = "js/jstree-master/dist"  -- directory containing 'jquery.jstree.js'
    , clckJSON2Path       = "js/json2"   -- directory containing 'json2.js'
    , clckTopDir          = Nothing      -- directory to store database, uploads, and other files
    , clckEnableAnalytics = False        -- enable Google Analytics
    , clckInitHook        = initHook     -- init hook that loads theme and plugins
    }

------------------------------------------------------------------------------
-- initHook
------------------------------------------------------------------------------

-- | This 'initHook' is used as the 'clckInitHook' field in
-- 'ClckwrksConfig'.
--
-- It will be called automatically by 'simpleClckwrks'. This hook
-- provides an opportunity to explicitly load a theme and some
-- plugins.
--
-- Note that the we generally always init 'clckPlugin' here.
initHook :: Text           -- ^ baseURI, e.g. http://example.org
         -> ClckState      -- ^ current 'ClckState'
         -> ClckwrksConfig -- ^ the 'ClckwrksConfig'
         -> IO (ClckState, ClckwrksConfig)
initHook baseURI clckState cc =
    do let p = plugins clckState
       addHandler p "promo" (redirector "/page/view-page-slug/8/promo")
       addHandler p "pawstars-thanks" (redirector "/page/view-page-slug/10/pawstars-thanks")
       addHandler p "pawstars-covers" (redirector "/page/view-page-slug/11/pawstars-covers")
       addHandler p "free-downloads" (redirector "/page/view-page/10/")
       addHandler p "cover-songs" (redirector "/page/view-page/11/")
       addHandler p ".well-known" letsencryptHandler
       initPlugin p baseURI authenticatePlugin
       initPlugin p "" clckPlugin
       initPlugin p "" pagePlugin
       initPlugin p "" mediaPlugin
--       initPlugin p "" mailingListPlugin
       setTheme p (Just theme)
       return (clckState, cc)

letsencryptHandler :: ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
letsencryptHandler _plugins paths =
    localRq (\rq -> rq { rqPaths = map unpack paths }) $
      serveDirectory DisableBrowsing [] "/tmp/webroot/.well-known"

promoHandler :: ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
promoHandler _plugins paths =
  localRq (\rq -> rq { rqPaths = map unpack paths }) $
    msum [ nullDir >> seeOther ("/page/view-page-slug/8/promo" :: Text) (toResponse ()) ]

redirector :: Text -> ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
redirector path _plugins paths =
  localRq (\rq -> rq { rqPaths = map unpack paths }) $
    msum [ nullDir >> seeOther path (toResponse ()) ]

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- getArgs
       f    <- parseArgs (clckwrksOpts clckwrksConfig) args
       simpleClckwrks =<< f clckwrksConfig
