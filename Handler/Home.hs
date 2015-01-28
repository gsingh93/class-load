module Handler.Home where

import Import
import Text.Julius (rawJS)
import Data.Text (pack, unpack)
import qualified Data.Set as Set

getHomeR :: Handler Html
getHomeR = do
  let classes = [("EECS 203", 3), ("EECS 215", 3), ("EECS 280", 3), ("EECS 281", 4), ("EECS 370", 3), ("EECS 373", 4), ("EECS 376", 3), ("EECS 381", 4), ("EECS 388", 3), ("EECS 445", 4), ("EECS 475", 3), ("EECS 482", 4), ("EECS 483", 4), ("EECS 484", 3), ("EECS 485", 3), ("EECS 489", 4), ("EECS 492", 3), ("EECS 494", 4)]
      classes' = map (\(s, i) -> (s ++ " (" ++ show i ++ ")", i)) classes
  defaultLayout $ do
    params <- reqGetParams <$> getRequest
    setTitle "Course Load Calculator"
    toWidget [julius|#{rawJS $ getClasses classes' $ map (unpack . snd) params};
|]
    $(widgetFile "elm")
    $(widgetFile "homepage")

getClasses :: [(String, Int)] -> [String] -> Text
getClasses classes checked =
  pack $ "var allCourses = { allCourses: [[" ++ coursesStr ++  "]," ++ show checked ++ " ]}"
    where coursesStr = foldl (\acc (s, i) ->
                               acc ++ "[\"" ++ s ++ "\", [" ++ (show i) ++
                               ", " ++ (if Set.member s selectedCourses then "true" else "false") ++ "]], " ) "" classes
          selectedCourses = Set.fromList checked
