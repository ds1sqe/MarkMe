-- MarkMe/Html/Internal.hs

module Html.Internal where

import           Numeric.Natural


-- * Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * Monoid and Semigroup

instance Semigroup Structure where
    (<>) s1 s2 =
        Structure (getStringFromStructure s1
          <> getStringFromStructure s2)


emptyStructure :: Structure
emptyStructure = Structure ""

instance Monoid Structure where
    mempty = emptyStructure

-- * Internal Function

getStringFromStructure :: Structure -> String
getStringFromStructure (Structure str) = str

element :: String -> String -> String
element tag content =
    "<"<> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape =
  let
    escapeChar esc =
        case esc of
         '<'  -> "&lt;"
         '>'  -> "&gt;"
         '&'  -> "&amp;"
         '"'  -> "&quot;"
         '\'' -> "&#39;"
         _    -> [esc]
  in
    concatMap escapeChar


-- * Exportable Function
appendTag :: Structure -> Structure -> Structure
appendTag tag1 tag2 =
    Structure (getStringFromStructure tag1 <> getStringFromStructure tag2)


-- * Tags

hTag :: Natural -> String -> Structure
hTag num = Structure . element ("h" <> show num) . escape

pTag :: String -> Structure
pTag = Structure . element "p" . escape

codeTag :: String -> Structure
codeTag = Structure . element "pre" . escape

ulTag :: [Structure] -> Structure
ulTag = Structure . element "ul" . concatMap (element "li" . getStringFromStructure)

olTag :: [Structure] -> Structure
olTag = Structure . element "ol" . concatMap (element "li" . getStringFromStructure)


render :: Html -> String
render html =
    case html of
        Html str -> str

-- Html wrapper
htmlTag :: Title -> Structure -> Html
htmlTag title content =
    Html (
      element "html" (
        element "head" (
          element "title" (escape title)) <>
        element "body"  (getStringFromStructure content)
     ))
