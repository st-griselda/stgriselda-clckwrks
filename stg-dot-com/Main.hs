{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}
module Main where

import Clckwrks             (ClckT, ClckURL, ClckPlugins, ClckwrksConfig(..), ClckState, TLSSettings(..), plugins)
import Clckwrks.GetOpts     (parseArgs, clckwrksOpts)
import Clckwrks.Server      (simpleClckwrks)
import Clckwrks.Plugin      (clckPlugin)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Page.Plugin (pagePlugin)
import Clckwrks.MailingList.Plugin (mailingListPlugin)
import Control.Monad        (msum)
import Data.Text            (Text, unpack)
import Happstack.Server     (Request(rqPaths), Response, ServerPartT, nullDir, seeOther, toResponse, localRq)
import Web.Plugins.Core     (addHandler, initPlugin, setTheme)
import System.Environment   (getArgs)
-- we use 'PackageImports' because the 'Theme' module is supplied by multiple packages
-- import "clckwrks-theme-bootstrap" Theme (theme)
import Theme (theme)

------------------------------------------------------------------------------
-- ClckwrksConfig
------------------------------------------------------------------------------

tls :: TLSSettings
tls = TLSSettings
      { clckTLSPort = 8443
      , clckTLSCert = "ssl/localhost.crt"
      , clckTLSKey  = "ssl/localhost.key"
      , clckTLSCA   = Nothing
      }

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
       addHandler p "promo" promoHandler
       initPlugin p baseURI authenticatePlugin
       initPlugin p "" clckPlugin
       initPlugin p "" pagePlugin
       initPlugin p "" mailingListPlugin
       setTheme p (Just theme)
       return (clckState, cc)


promoHandler :: ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
promoHandler _plugins paths =
  localRq (\rq -> rq { rqPaths = map unpack paths }) $
    msum [ nullDir >> seeOther ("/page/view-page-slug/8/promo" :: Text) (toResponse ()) ]

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- getArgs
       f    <- parseArgs (clckwrksOpts clckwrksConfig) args
       simpleClckwrks =<< f clckwrksConfig
