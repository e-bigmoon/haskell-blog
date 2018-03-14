{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Yaml    (FromJSON, decodeFile)
import           GHC.Generics

fromConfig :: (FromJSON a) => FilePath -> IO (Maybe a)
fromConfig = decodeFile

data Style = Style
  { headerColour            :: Colour
  , headThemeColour         :: HexColour
  , footerColour            :: Colour
  , footerBtnColour         :: Colour
  , footerLinkColour        :: Colour
  , navbarTextColourDesktop :: Colour
  , navbarTextColourMobile  :: Colour
  }
  deriving Generic

type Colour = String
type HexColour = String

data General = General
  { baseUrl   :: String
  , headTitle :: String
  , siteTitle :: String
  , paginate  :: Int
  }
  deriving Generic

data Feed = Feed
  { title       :: String
  , description :: String
  , authorName  :: String
  , authorEmail :: String
  , root        :: String
  }
  deriving Generic

data Site = Site
  { general :: General
  , feed    :: Feed
  , style   :: Style
  }
  deriving Generic

instance FromJSON Style
instance FromJSON General
instance FromJSON Feed
instance FromJSON Site







