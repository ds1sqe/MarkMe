module Html (
    Html,
    Structure,
    Title,
    h1Tag,
    pTag,
    appendTag,
    render,
    htmlTag,
)
where

-- type

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- internal function
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


-- exportable function
appendTag :: Structure -> Structure -> Structure
appendTag tag1 tag2 =
    Structure (getStringFromStructure tag1 <> getStringFromStructure tag2)


-- tags

h1Tag:: String -> Structure
h1Tag = Structure . element "h1" . escape

pTag:: String -> Structure
pTag = Structure . element "p" . escape


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
