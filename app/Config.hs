module Config
  ( Feed
  , Site
  )
where

import Data.Extensible
import RIO

type Site =
  Record
    [ "general" >: General,
      "feed" >: Feed,
      "style" >: Style
    ]

type Feed =
  Record
    [ "title" >: String,
      "description" >: String,
      "authorName" >: String,
      "authorEmail" >: String,
      "root" >: String
    ]

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
