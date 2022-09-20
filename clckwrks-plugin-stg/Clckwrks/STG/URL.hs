{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.STG.URL where

import Data.Data (Data, Typeable)
import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import Clckwrks.STG.Acid          (STGId(..))
import Clckwrks.STG.Types         (Slug(..))
import Web.Routes.TH               (derivePathInfo)

data STGAdminURL
    = EditSTG STGId
    | PreviewSTG STGId
    | STGs
    | NewSTG
    | NewPost
    | EditFeedConfig
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''STGAdminURL)
$(derivePathInfo ''STGAdminURL)

data STGURL
    = ViewSTG STGId
    | ViewSTGSlug STGId Slug
    | Blog
    | AtomFeed
    | STGAdmin STGAdminURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''STGURL)
$(derivePathInfo ''STGURL)
