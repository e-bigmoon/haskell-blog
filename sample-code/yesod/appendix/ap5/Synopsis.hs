#!/usr/bin/env stack
-- stack script --resolver lts-16.29
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML

main :: IO ()
main = do
    -- readFile will throw any parse errors as runtime exceptions
    -- def uses the default settings
    Document prologue root epilogue <- readFile def "input.xml"

    -- root is the root element of the document, let's modify it
    let root' = transform root

    -- And now we write out. Let's indent our output
    writeFile def
        { rsPretty = True
        } "output.html" $ Document prologue root' epilogue

-- We'll turn out <document> into an XHTML document
transform :: Element -> Element
transform (Element _name attrs children) = Element "html" M.empty
    [xml|
        <head>
            <title>
                $maybe title <- M.lookup "title" attrs
                    \#{title}
                $nothing
                    Untitled Document
        <body>
            $forall child <- children
                ^{goNode child}
    |]

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element "para" attrs children) =
    Element "p" attrs $ concatMap goNode children
goElem (Element "em" attrs children) =
    Element "i" attrs $ concatMap goNode children
goElem (Element "strong" attrs children) =
    Element "b" attrs $ concatMap goNode children
goElem (Element "image" attrs _children) =
    Element "img" (fixAttr attrs) [] -- images can't have children
  where
    fixAttr mattrs
        | "href" `M.member` mattrs  = M.delete "href" $ M.insert "src" (mattrs M.! "href") mattrs
        | otherwise                 = mattrs
goElem (Element name attrs children) =
    -- don't know what to do, just pass it through...
    Element name attrs $ concatMap goNode children