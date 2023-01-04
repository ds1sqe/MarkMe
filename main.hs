-- import
import           Html
-- monoid

-- main
html :: Html
html =
  htmlTag "Title"
    (appendTag ( h1Tag "Heading 1")
        ( appendTag (pTag "Paragraph 1") (pTag "Paragraph 2"))
    )

main :: IO ()
main = putStrLn (render html)
