-- MarkMe/Markup/Internal.hs

module Markup.Internal where

import           Data.Maybe      ( maybeToList )

import           Numeric.Natural

-- * Types

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

-- * Internal Functions

trim :: String -> String
trim = unwords . words

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- completed or a input was invailid
    [] -> maybeToList context

    -- Heading
    ('*' : ' ' : line) : rest ->
        maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

    -- UnorderedList
    ('-' : ' ' : line) : rest ->
        case context of
            Just (UnorderedList list) ->
                 parseLines (Just (UnorderedList (list <> [trim line]))) rest
            _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- OrderedList
    ('#' : ' ' : line) : rest ->
        case context of
            Just (OrderedList list) ->
                 parseLines (Just (OrderedList (list <> [trim line]))) rest
            _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- CodeBlock
    ('>' : ' ' : line) : rest ->
        case context of
            Just (CodeBlock code) ->
                 parseLines (Just (CodeBlock (code <> [trim line]))) rest
            _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- Paragraph
    currentLine : rest ->
        let line = trim currentLine
        in if line == ""
           then maybe id (:) context (parseLines Nothing rest)
           else
             case context of
             Just (Paragraph paragraph) -> parseLines (Just -- if not empty,
                    (Paragraph (unwords [paragraph,line]))) rest -- append line to context and parse rest
             _ ->   maybe id (:) context (parseLines (Just (Paragraph line)) rest)
             -- if empty like,

-- * Exportable Functions

parse :: String -> Document
parse = parseLines Nothing . lines
