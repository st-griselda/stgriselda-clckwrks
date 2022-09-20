{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, QuasiQuotes #-}
module Theme where

import Clckwrks
import Clckwrks.Authenticate.Plugin  (authenticatePlugin)
import Clckwrks.Authenticate.URL     (AuthURL(Auth))
import Clckwrks.Types                (NamedLink(..))
import Clckwrks.NavBar.API           (getNavBarData)
import Clckwrks.NavBar.Types         (NavBar(..), NavBarItem(..))
import Clckwrks.Monad
import Control.Monad.State           (get)
import Clckwrks.ProfileData.Acid     (HasRole(..))
import Data.Maybe                    (fromMaybe)
import qualified Data.Set            as Set
import           Data.Text.Lazy      (Text)
import qualified Data.Text           as T
import Happstack.Authenticate.Password.URL (PasswordURL(UsernamePasswordCtrl), passwordAuthenticationMethod)
import HSP.XMLGenerator
import HSP.XML                       (XML)
import Language.Haskell.HSX.QQ       (hsx)
import Paths_clckwrks_theme_stg      (getDataDir)
import Web.Plugins.Core              (pluginName, getPluginRouteFn)

theme :: Theme
theme = Theme
    { themeName      = "stg"
    , themeStyles    = [standardStyle, homepageStyle]
    , themeDataDir   = getDataDir
    }

-- | function te generate the navigation bar
genNavBar :: GenXML (Clck ClckURL)
genNavBar =
    do menu  <- lift getNavBarData
       mName <- query GetSiteName
       navBarHTML (fromMaybe "St. Griselda" mName) menu

navBarHTML :: T.Text -- ^ brand
           -> NavBar -- ^ nav bar links
           -> GenXML (Clck ClckURL)
navBarHTML brand (NavBar navBarItems) = [hsx|
  <nav class="nav">
   <div class="nav-left">
    <a class="nav-item is-brand" href="/">
     <img src=(ThemeData "data/imgs/logo-header-black.png") alt="St. Griselda Logo" />
     <span>St. Griselda</span>
    </a>
   </div>
   <span class="nav-toggle" onClick="$(this).toggleClass('is-active'); $('.nav-menu').toggleClass('is-active'); ">
     <span></span>
     <span></span>
     <span></span>
   </span>

   <div class="nav-center">
   </div>

   <div class="nav-right nav-menu">
    <% mapM mkNavBarItem navBarItems %>
   </div>

  </nav>
  |]

mkNavBarItem :: NavBarItem -> GenXML (Clck ClckURL)
mkNavBarItem (NBLink (NamedLink ttl lnk)) =
    [hsx| <a class="nav-item" href=lnk><% ttl %></a> |]


commonTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                    , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                    ) =>
                    T.Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
commonTemplate ttl hdr bdy = do
    p <- plugins <$> get
    ~(Just authRouteFn) <- getPluginRouteFn p (pluginName authenticatePlugin)
    [hsx|
    <html lang="en">
     <head>
      <meta charset="utf-8" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      -- favicon
     <link rel="apple-touch-icon" sizes="57x57" href=(ThemeData "data/favicon/apple-touch-icon-57x57.png") />
     <link rel="apple-touch-icon" sizes="60x60" href=(ThemeData "data/favicon/apple-touch-icon-60x60.png") />
     <link rel="apple-touch-icon" sizes="72x72" href=(ThemeData "data/favicon/apple-touch-icon-72x72.png") />
     <link rel="apple-touch-icon" sizes="76x76" href=(ThemeData "data/favicon/apple-touch-icon-76x76.png") />
     <link rel="apple-touch-icon" sizes="114x114" href=(ThemeData "data/favicon/apple-touch-icon-114x114.png") />
     <link rel="apple-touch-icon" sizes="120x120" href=(ThemeData "data/favicon/apple-touch-icon-120x120.png") />
     <link rel="apple-touch-icon" sizes="144x144" href=(ThemeData "data/favicon/apple-touch-icon-144x144.png") />
     <link rel="apple-touch-icon" sizes="152x152" href=(ThemeData "data/favicon/apple-touch-icon-152x152.png") />
     <link rel="apple-touch-icon" sizes="180x180" href=(ThemeData "data/favicon/apple-touch-icon-180x180.png") />
     <link rel="icon" type="image/png" href=(ThemeData "data/favicon/favicon-32x32.png") sizes="32x32" />
     <link rel="icon" type="image/png" href=(ThemeData "data/favicon/android-chrome-192x192.png") sizes="192x192" />
     <link rel="icon" type="image/png" href=(ThemeData "data/favicon/favicon-96x96.png") sizes="96x96" />
     <link rel="icon" type="image/png" href=(ThemeData "data/favicon/favicon-16x16.png") sizes="16x16" />
     <link rel="manifest" href=(ThemeData "data/favicon/manifest.json") />
     <meta name="apple-mobile-web-app-title" content="St. Griselda" />
     <meta name="application-name" content="St. Griselda" />
     <meta name="msapplication-TileColor" content="#da532c" />
     <meta name="msapplication-TileImage" content="/mstile-144x144.png" />
     <meta name="theme-color" content="#ffffff" />
      -- the meta tags must come first
      <title><% ttl %></title>
--      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/bootstrap.min.css")  />
--      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/clckwrks-theme.min.css")  />
      <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/bulma/0.2.1/css/bulma.css" />
      -- <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/hscolour.css") />
      -- jquery
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
      -- bootstrap
      <script src=(ThemeData "data/js/bootstrap.min.js")></script>
      -- angular
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
      <script src=(JS ClckwrksApp)></script>
      <script src=(authRouteFn (Auth Controllers) [])></script>
      <% hdr %>
      <% googleAnalytics %>
      -- Facebook Pixel Code
      <script>
       !function(f,b,e,v,n,t,s){if(f.fbq)return;n=f.fbq=function(){n.callMethod?
       n.callMethod.apply(n,arguments):n.queue.push(arguments)};if(!f._fbq)f._fbq=n;
       n.push=n;n.loaded=!0;n.version='2.0';n.queue=[];t=b.createElement(e);t.async=!0;
       t.src=v;s=b.getElementsByTagName(e)[0];s.parentNode.insertBefore(t,s)}(window,
       document,'script','https://connect.facebook.net/en_US/fbevents.js');
       fbq('init', '220030181784624'); // Insert your pixel ID here.
       fbq('track', 'PageView');
      </script>
      <noscript><img height="1" width="1" style="display:none"
      src="https://www.facebook.com/tr?id=220030181784624&ev=PageView&noscript=1"
      /></noscript>
      -- End Facebook Pixel Code

     </head>
     <body ng-app="clckwrksApp" ng-controller="AuthenticationCtrl as auth">
       <% heading %>
       <% bdy %>
{-
       <footer id="footer" class="row footer">
        <div class="col-md-1"></div>
        <div class="col-md-8">
          <p class="small">© 2016, St Griselda. All Rights Reserved.</p>
        </div>
      </footer>
        -}
     </body>
    </html>
    |]

videoSplash :: GenXML (Clck ClckURL)
videoSplash = [hsx|
  <div class="container-fluid">
   <div class="row">
--    <div class="col-md-12">
--     <div class="header-unit">
      <div id="video-container">
       <video autoplay="true" loop="" class="fillWidth">
        <source src=(ThemeData "data/imgs/NLSM-loop.webm") type="video/webm" />
--        <source src="http://yourwebsite.com/your-video-file.mp4" type="video/mp4"/>
--        <source src="http://yourwebsite.com/your-video-file.ogg" type="video/ogg"/>
        <p>Your browser does not support the video tag. I suggest you upgrade your browser.</p>
       </video>
      </div>
      <div class="video-overlay">
       <h1>St. Griselda</h1>

      </div>
 -- end video-container
--     </div> -- end .header-unit
--    </div>
   </div>
  </div>
  |]

videoSplashTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                    , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                    ) =>
                    T.Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
videoSplashTemplate ttl hdr bdy = do
    p <- plugins <$> get
    ~(Just authRouteFn) <- getPluginRouteFn p (pluginName authenticatePlugin)
    [hsx|
    <html lang="en">
     <head>
      <meta charset="utf-8" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      -- favicon
      <link rel="apple-touch-icon" sizes="57x57" href=(ThemeData "data/favicon/apple-touch-icon-57x57.png") />
      <link rel="apple-touch-icon" sizes="60x60" href=(ThemeData "data/favicon/apple-touch-icon-60x60.png") />
      <link rel="apple-touch-icon" sizes="72x72" href=(ThemeData "data/favicon/apple-touch-icon-72x72.png") />
      <link rel="apple-touch-icon" sizes="76x76" href=(ThemeData "data/favicon/apple-touch-icon-76x76.png") />
      <link rel="apple-touch-icon" sizes="114x114" href=(ThemeData "data/favicon/apple-touch-icon-114x114.png") />
      <link rel="apple-touch-icon" sizes="120x120" href=(ThemeData "data/favicon/apple-touch-icon-120x120.png") />
      <link rel="apple-touch-icon" sizes="144x144" href=(ThemeData "data/favicon/apple-touch-icon-144x144.png") />
      <link rel="apple-touch-icon" sizes="152x152" href=(ThemeData "data/favicon/apple-touch-icon-152x152.png") />
      <link rel="apple-touch-icon" sizes="180x180" href=(ThemeData "data/favicon/apple-touch-icon-180x180.png") />
      <link rel="icon" type="image/png" href=(ThemeData "data/favicon/favicon-32x32.png") sizes="32x32" />
      <link rel="icon" type="image/png" href=(ThemeData "data/favicon/android-chrome-192x192.png") sizes="192x192" />
      <link rel="icon" type="image/png" href=(ThemeData "data/favicon/favicon-96x96.png") sizes="96x96" />
      <link rel="icon" type="image/png" href=(ThemeData "data/favicon/favicon-16x16.png") sizes="16x16" />
      <link rel="manifest" href=(ThemeData "data/favicon/manifest.json") />
      <meta name="apple-mobile-web-app-title" content="St. Griselda" />
      <meta name="application-name" content="St. Griselda" />
      <meta name="msapplication-TileColor" content="#da532c" />
      <meta name="msapplication-TileImage" content="/mstile-144x144.png" />
      <meta name="theme-color" content="#ffffff" />
      -- the meta tags must come first
      <title><% ttl %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/bootstrap.min.css")  />
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/clckwrks-theme.min.css")  />
      -- <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/hscolour.css") />
      -- jquery
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
      -- bootstrap
      <script src=(ThemeData "data/js/bootstrap.min.js")></script>
      -- angular
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
      <script src=(JS ClckwrksApp)></script>
      <script src=(authRouteFn (Auth Controllers) [])></script>
      <% hdr %>
      <% googleAnalytics %>
     </head>
     <body ng-app="clckwrksApp" ng-controller="AuthenticationCtrl as auth">
       <% genNavBar %>
       <% videoSplash %>
       <div class="container-fluid">
        <div class="row">
         <div class="col-md-1"></div>
         <div class="col-md-10 content-section">
          <div class="row">
           <div class="col-md-8 widget text-section">
            <% bdy %>
           </div>
          </div>
         </div>
        </div>
       </div>
{-
       <footer id="footer" class="row footer">
        <div class="col-md-1"></div>
        <div class="col-md-8">
          <p class="small">© 2015, St Griselda. All Rights Reserved.</p>
        </div>
       </footer>
        -}
     </body>
    </html>
    |]

standardTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                    , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                    ) =>
                    T.Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
standardTemplate ttl hdr bdy = do
  commonTemplate ttl hdr [hsx|
                             <section class="section">
                               <div class="container">
                                <% bdy %>
                               </div>
                             </section>
                             |]
{-
  commonTemplate ttl hdr [hsx|
     <div class="container-fluid">
      <div class="row">
       <div class="col-md-1"></div>

       <div class="col-md-10 content-section">
        <div class="row">
         <div class="col-md-7 widget text-section">
           <% bdy %>
         </div>
         <div class="col-md-1"></div>
         <div class="col-md-4 widget social-media-section">
           <h1>Social</h1>
           <p><a href="https://patreon.com/stgriselda"><img src=(ThemeData "data/imgs/patreon_logo.png") /></a></p>
           <p><a href="https://hearthis.at/stgriselda"><img src=(ThemeData "data/imgs/hearthis.png") /></a></p>
           <p><a href="https://twitter.com/stgriselda"><img src=(ThemeData "data/imgs/TwitterLogo.png") /></a></p>
           -- <p><a href="https://instagram.com/lord_gothington"><img src=(ThemeData "data/imgs/instagram.png") /></a></p>
           <p><a href="https://www.facebook.com/StGriselda/"><img src=(ThemeData "data/imgs/facebook.png") /></a></p>
         </div>

        </div>
       </div>
      </div>
     </div>
   |]
-}
  {-
    p <- plugins <$> get
    (Just authRouteFn) <- getPluginRouteFn p (pluginName authenticatePlugin)
    [hsx|
    <html>
     <head>
      <meta charset="utf-8" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      -- the meta tags must come first
      <title><% ttl %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/bootstrap.min.css")  />
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/clckwrks-theme.min.css")  />
      -- <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/hscolour.css") />
      -- jquery
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
      -- bootstrap
      <script src=(ThemeData "data/js/bootstrap.min.js")></script>
      -- angular
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
      <script src=(JS ClckwrksApp)></script>
      <script src=(authRouteFn (Auth Controllers) [])></script>
      <% hdr %>
      <% googleAnalytics %>
     </head>
     <body ng-app="clckwrksApp" ng-controller="AuthenticationCtrl as auth">
      <div id="wrap">
       <% genNavBar %>
       <div class="container">
         <div class="row">
          <div class="span8">
           <% bdy %>
          </div>
         </div>
         <div id="push"></div>
       </div>
      </div>

      <footer id="footer" class="footer">
      </footer>

     </body>
    </html>
    |]

-}

heading :: GenXML (Clck ClckURL)
heading = genNavBar
  {- [hsx|
{-  <div>
   <div class="columns">
    <div class="column">
     <span>St Griselda</span><img src=(ThemeData "data/imgs/logo-header.png") alt="stg-logo" />
    </div>
   </div>
    -}
   <% genNavBar %>
--  </div>
  |] -}

homepageTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                    , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                    ) =>
                    T.Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
homepageTemplate ttl hdr bdy = do
  commonTemplate ttl hdr [hsx| <% bdy %>
                             |]
  {-
    p <- plugins <$> get
    (Just authRouteFn) <- getPluginRouteFn p (pluginName authenticatePlugin)
    [hsx|
    <html>
     <head>
      <meta charset="utf-8" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      -- the meta tags must come first
      <title><% ttl %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/bootstrap.min.css")  />
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/clckwrks-theme.min.css")  />
      -- <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/hscolour.css") />
      -- jquery
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
      -- bootstrap
      <script src=(ThemeData "data/js/bootstrap.min.js")></script>
      -- angular
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
      <script src=(JS ClckwrksApp)></script>
      <script src=(authRouteFn (Auth Controllers) [])></script>
      <% hdr %>
      <% googleAnalytics %>
     </head>
     <body ng-app="clckwrksApp" ng-controller="AuthenticationCtrl as auth">
      <div id="wrap">
       <% genNavBar %>
       <div class="jumbotron">
        <div class="container">
         <img src=(ThemeData "data/imgs/jumbotron.png") />
        </div>
       </div>
       <div class="container">
         <div class="row">
          <div class="span8">
           <% bdy %>
          </div>
         </div>
         <div id="push"></div>
       </div>
      </div>

      <footer id="footer" class="footer">
      </footer>

     </body>
    </html>
    |]
-}
standardStyle :: ThemeStyle
standardStyle = ThemeStyle
    { themeStyleName        = "standard"
    , themeStyleDescription = "standard view"
    , themeStylePreview     = Nothing
    , themeStyleTemplate    = standardTemplate
    }

homepageStyle :: ThemeStyle
homepageStyle = ThemeStyle
    { themeStyleName        = "homepage"
    , themeStyleDescription = "homepage"
    , themeStylePreview     = Nothing
    , themeStyleTemplate    = homepageTemplate
    }
