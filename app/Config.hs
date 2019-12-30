{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Data.Extensible
import Data.Yaml (FromJSON, ParseException, decodeFileEither)

fromConfig :: (FromJSON a) => FilePath -> IO (Either ParseException a)
fromConfig = decodeFileEither

type Style =
  Record
    [ "headerColour" >: Colour,
      "headThemeColour" >: HexColour,
      "footerColour" >: Colour,
      "footerBtnColour" >: Colour,
      "footerLinkColour" >: Colour,
      "navbarTextColourDesktop" >: Colour,
      "navbarTextColourMobile" >: Colour,
      "shareButtonColour" >: Colour,
      "shareButtonSmallColour" >: Colour
    ]

type Colour = String

type HexColour = String

type General =
  Record
    [ "baseUrl" >: String,
      "headTitle" >: String,
      "siteTitle" >: String,
      "paginate" >: Int
    ]

type Feed =
  Record
    [ "title" >: String,
      "description" >: String,
      "authorName" >: String,
      "authorEmail" >: String,
      "root" >: String
    ]

type Site =
  Record
    [ "general" >: General,
      "feed" >: Feed,
      "style" >: Style
    ]
