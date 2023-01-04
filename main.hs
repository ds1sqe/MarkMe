import           Control.Monad      ( when )

import qualified Convert            as Cv

import           Html               ( Title )
import qualified Html               as Hl

import qualified Markup             as Mk

import           System.Directory   ( doesFileExist )
import           System.Environment ( getArgs )
import           System.IO          ( getContents, readFile, writeFile )


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= \content  ->
        putStrLn (Cv.process "Empty Title" content)

    [input,output] -> readFile input >>= \content ->
        doesFileExist output >>= \isExist ->
            let write = writeFile output (Cv.process input content)
            in if isExist
            then whenIO confirm write
            else write

    _ -> putStrLn "Usage: ..."

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
     "y" -> pure True
     "n" -> pure False
     _ -> putStrLn "Invalid response. use y or n" *>
          confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    when result action
