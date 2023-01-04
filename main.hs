main = putStrLn html

html = createHtml "Hello title" "Hello content"

createHtml title content = tagHtml (tagHead (tagTitle (tagH1 title)) <> tagBody (tagP content))

element :: String -> String -> String
element tag content =
    "<"<> tag <> ">" <> content <> "</" <> tag <> ">"

tagHtml = element "html"
tagBody = element "body"
tagHead = element "head"
tagTitle = element "title"
tagP = element "p"
tagH1 = element "h1"
