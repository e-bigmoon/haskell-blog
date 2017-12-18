{-# LANGUAGE DeriveGeneric #-}
module Config
where

import GHC.Generics
import Data.Yaml (FromJSON, decodeFile)


fromConfig :: (FromJSON a) => FilePath -> IO (Maybe a)
fromConfig =
  decodeFile


data Style =
  Style { header_colour :: Colour
        , head_theme_colour :: HexColour
        , footer_colour :: Colour
        , footer_btn_colour :: Colour
        , footer_link_colour :: Colour
        , navbar_text_colour_desktop :: Colour
        , navbar_text_colour_mobile :: Colour
        }
  deriving Generic


type Colour =
  String


type HexColour =
  String


data General =
  General { base_url :: String
          , head_title :: String
          , site_title :: String
          , paginate :: Int
          }
  deriving Generic


data Feed =
  Feed { title :: String
       , description :: String
       , author_name :: String
       , author_email:: String
       , root :: String
       }
  deriving Generic


data Site =
  Site { general :: General
       , feed :: Feed
       , style :: Style
       }
  deriving Generic


instance FromJSON Style
instance FromJSON General
instance FromJSON Feed
instance FromJSON Site
