module Handler.Home where

import Import
import Text.Julius (rawJS)
import Data.Text (pack)

getHomeR :: Handler Html
getHomeR = do
  let classes = [("EECS 280", 3), ("EECS 281", 4), ("EECS 482", 4), ("EECS 483", 4)]
  defaultLayout $ do
    setTitle "Course Load Calculator"
    toWidget [julius|#{rawJS $ getClasses classes};
    |]
    $(widgetFile "elm")
    $(widgetFile "homepage")

getClasses :: [(String, Int)]-> Text
getClasses classes = pack $ "var allCourses = { allCourses: [" ++ coursesStr ++  "] }"
  where coursesStr = foldl (\acc (s, i) ->
                             acc ++ "[\"" ++ s ++ "\", " ++ (show i) ++ "], " ) "" classes
