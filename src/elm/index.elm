import Color (..)
import Markdown
import Dict
import Maybe
import Set
import List
import List ((::))
import Window
import Graphics.Element (Element, container, flow, down, color, middle,
                         right, spacer, midTop)
import Graphics.Input as Input
import Text
import Text (plainText, fromString, centered, height, bold, asText, typeface)
import Signal (..)

type alias State = { courseInfo : Dict.Dict String (Int, Bool),
                     selectedCourses : Set.Set String}

zip : List a -> List b -> List (a,b)
zip listX listY =
  case (listX, listY) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    (  _  ,   _  ) -> []


headerHeight : Int
headerHeight = 100

allCourses : List (String, Int)
allCourses = [("EECS 280", 3), ("EECS 281", 5), ("EECS 482", 5),
              ("EECS 483", 4)]

makeDict : Dict.Dict String (Int, Bool)
makeDict = List.foldl (\(k, v) d -> Dict.insert k (v, False) d)
           Dict.empty allCourses

updateState : (String, Bool) -> State -> State
updateState (s, b) state =
  let newCourseInfo = Dict.update s (\(Just (credits, _)) -> Just (credits, b))
                      state.courseInfo
  in case b of
      True -> { courseInfo = newCourseInfo,
                selectedCourses = Set.insert s state.selectedCourses}
      False -> { courseInfo = newCourseInfo,
                 selectedCourses = Set.remove s state.selectedCourses}

getState : Signal State
getState = foldp updateState { courseInfo = makeDict,
                               selectedCourses = Set.empty} (subscribe check)

box =
  let dim = 200
  in color white <| container dim dim middle
     <| typeface ["Helvetica"] >> height 30 >> Text.color (rgb 56 169 255)
     >> centered
     <| fromString "EECS 280"

main : Signal Element
main = view <~ Window.dimensions ~ getState

view : (Int, Int) -> State -> Element
view (w, h) s = flow down [header w headerHeight,
                           mainContainer w h s]

mainContainer : Int -> Int -> State -> Element
mainContainer w h s = color grey (container w (h - headerHeight) middle
                                  <| appContainer s 200 500)

appContainer : State -> Int -> Int -> Element
appContainer s w h =
  let gap = 20
  in flow right [checkboxContainer s.courseInfo w h,
                 spacer gap 1,
                 selectedCoursesContainer s.selectedCourses w h,
                 spacer gap 1,
                 resultsContainer s w h]

checkboxContainer : Dict.Dict String (Int, Bool) -> Int -> Int -> Element
checkboxContainer courseInfo w h =
  let courses = (zip (snd <| List.unzip <| Dict.values courseInfo)
               (Dict.keys courseInfo))
  in color darkGrey <| container w h midTop
     <|flow down
     <| List.map (uncurry createCheckbox) courses

selectedCoursesContainer : Set.Set String -> Int -> Int -> Element
selectedCoursesContainer courses w h =
  let selectedCourses = (List.map (plainText >> container 100 30 middle)
                         (Set.toList courses))
      foo = [box]
  in color darkGrey <| container w h midTop <| flow down selectedCourses

resultsContainer : State -> Int -> Int -> Element
resultsContainer state w h =
  let (Just sum) = Set.foldl (\s (Just acc) -> Maybe.map (\(i, _) -> i + acc)
                                        (Dict.get s state.courseInfo))
                   (Just 0) state.selectedCourses
  in color darkGrey <| container w h midTop <| flow down [
    plainText <| "Workload score: " ++ toString sum,
    Markdown.toElement <| "Semester rating: " ++ resultsMessage sum]

resultsMessage : Int -> String
resultsMessage sum = if | sum <= 5 -> "Easy"
                        | sum > 5 && sum <= 10 -> "Not too bad"
                        | sum > 10 && sum <= 15 -> "You'll be busy"
                        | sum > 15 -> "It's going to be a tough semester..."

header : Int -> Int -> Element
header w h = color darkGrey (container w h middle titleText)

titleText : Element
titleText = centered (bold (height 40 (fromString "Class Load Calculator")))

check : Channel (String, Bool)
check = channel ("", False)

createCheckbox : Bool -> String -> Element
createCheckbox checked label =
  flow right [container 30 30 middle
              (Input.checkbox (\b -> send check (label, b)) checked),
              container 100 30 middle (plainText label)]
