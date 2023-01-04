module Convert where

import           Html   ( Title )
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

    Mk.UnorderedList list ->
        Hl.ulTag $ map Hl.pTag list

    Mk.OrderedList list ->
        Hl.olTag $ map Hl.pTag list

    Mk.CodeBlock codes ->
        Hl.codeTag (unlines codes)


process :: Title -> String -> String
process title = Hl.render . convert title . Mk.parse
