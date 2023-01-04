module Convert where

import qualified Html   as Hl

import qualified Markup as Mk


convert :: Hl.Title -> Mk.Document -> Hl.Html
convert title = Hl.htmlTag title . foldMap convertMarkupToHtml


convertMarkupToHtml :: Mk.Structure -> Hl.Structure
convertMarkupToHtml structure =
  case structure of
    Mk.Heading n str ->
        Hl.hTag n str

    Mk.Paragraph str ->
        Hl.pTag str

    Mk.UnorderedList [str] ->
        Hl.ulTag $ map Hl.pTag [str]

    Mk.OrderedList [str] ->
        Hl.olTag $ map Hl.pTag [str]

    Mk.CodeBlock [str]->
        Hl.codeTag (unlines [str])
