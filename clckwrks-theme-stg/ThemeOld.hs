#if 0
-- | helper function to generate a navigation bar from the navigation bar data
navBarHTML :: T.Text   -- ^ brand
           -> NavBar -- ^ navigation bar links
           -> GenXML (Clck ClckURL)
navBarHTML brand (NavBar menuItems) = [hsx|
 <nav class="navbar navbar-default">
  <div class="container-fluid">
    -- Brand and toggle get grouped for better mobile display
    <div class="col-md-1"></div>
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
--      <a class="navbar-brand" href="/"><% brand %></a>
    </div>

    -- Collect the nav links, forms, and other content for toggling
    <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1" ng-show="!isAuthenticated">
      -- this is where actual menu things go
      <ul class="nav navbar-nav">
        <% mapM mkNavBarItem menuItems %>
      </ul>
{-
      <span class="" ng-controller="UsernamePasswordCtrl">
       <up-login-inline />
      </span>

      -- navbar-text would make more sense than navbar-form, but it shifts the images funny. :-/
      <span class="navbar-left navbar-btn" ng-controller="OpenIdCtrl" ng-show="!isAuthenticated">
       <openid-google />
      </span>
      <span class="navbar-left navbar-btn" ng-controller="OpenIdCtrl" ng-show="!isAuthenticated">
       <openid-yahoo />
      </span>

      <span up-authenticated=True class="navbar-left navbar-form">
       <a ng-click="logout()" href="">Logout {{claims.user.username}}</a>
      </span>
-}
    </div> -- /.navbar-collapse
  </div>  -- /.container-fluid
 </nav>
    |]

mkNavBarItem :: NavBarItem -> GenXML (Clck ClckURL)
mkNavBarItem (NBLink (NamedLink ttl lnk)) =
    [hsx| <li><a href=lnk><% ttl %></a></li> |]

