-- import

-- type

newtype Html = Html String
newtype Structure = Structure String

type Title = String

-- monoid

-- functions

getStringFromStructure :: Structure -> String
getStringFromStructure (Structure str) = str

element :: String -> String -> String
element tag content =
    "<"<> tag <> ">" <> content <> "</" <> tag <> ">"

appendTag :: Structure -> Structure -> Structure
appendTag (Structure a) (Structure b) =
    Structure (a <> b)

-- Html wrapper
tagHtml :: Title -> Structure -> Html
tagHtml title content =
    Html (
      element "html" (
        element "head" (
          element "title" title) <>
        element "body" ( getStringFromStructure content)
     ))

-- tags
tagP:: String -> Structure
tagP = Structure . element "p"

tagH1 :: String -> Structure
tagH1 = Structure .  element "h1"

render :: Html -> String
render html =
    case html of
        Html str -> str

--
html :: Html
html =
  tagHtml "Title"
    (appendTag ( tagH1 "Heading 1")
        ( appendTag (tagP "Paragraph 1") (tagP "Paragraph 2"))
    )

-- main

main :: IO ()
main = putStrLn (render html)
