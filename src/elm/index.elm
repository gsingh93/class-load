import Html (Html, fromElement)
import Color (..)
import Debug
import Dict
import List
import List ((::))
import Maybe
import Set
import Window
import Graphics.Element (Element, container, flow, down, color, middle,
                         right, spacer, midTop, width)
import Graphics.Input as Input
import Text
import Text (plainText, fromString, centered, height, bold, asText, typeface, link, leftAligned)
import Signal
import Signal (Signal, (<~), (~), foldp)

{-
The view updates based on the following signals:
 - A window resize
 - A state change
 - Checking/unchecking a checkbox

A state change occurs when a checkbox is checked/unchecked
 - getState gets the name of the course from the signal, flips the "checked" Bool in State.courseInfo, and adds/removes the course name from State.selectedCourses
-}

{---------- State functions ---------}

type alias State = {
  -- A mapping of course name to (credits, checked) tuple
  courseInfo : Dict.Dict String (Int, Bool),
  -- A Set of the names of the selected courses
  selectedCourses : Set.Set String}

-- Update the state when a checkbox is clicked
updateState : Update -> State -> State
updateState update state =
    case update of
      Click s -> 
          let newCourseInfo = Dict.update s (\(Just (credits, b)) -> Just (credits, not b))
                              state.courseInfo
              (Just (_, b)) = Dict.get s state.courseInfo
          in case not b of
               True -> { courseInfo = newCourseInfo,
                         selectedCourses = Set.insert s state.selectedCourses}
               False -> { courseInfo = newCourseInfo,
                          selectedCourses = Set.remove s state.selectedCourses}
      Init l -> { courseInfo = Dict.fromList <| List.map (\(s, (i, b)) -> (s ++ " (" ++ toString i ++ ")", (i, b))) <| fst l, selectedCourses = Set.fromList <| snd l }

type Update = Init (List (String, (Int, Bool)), List String) | Click String
getState : Signal State
getState = foldp updateState { courseInfo = Dict.empty,
                               selectedCourses = Set.empty} (Signal.merge (Init <~ allCourses) (Click <~ (Signal.subscribe click)))

{---------- Util functions and constants ----------}

zip : List a -> List b -> List (a,b)
zip listX listY =
  case (listX, listY) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    (  _  ,   _  ) -> []

genPermalink : State -> Element
genPermalink state = centered <| link (Dict.foldl (\k (_, b) acc -> let k_ : String
                                                                        k_ = k
                                                                    in if b then acc ++ "course=" ++ k_ ++ "&" else acc) "?" state.courseInfo) (fromString "Permalink")

port title : String
port title = "Course Load Calculator"

headerHeight : Int
headerHeight = 100

titleText : Element
titleText = typeface ["sans-serif"] >> Text.color color3 >> height 40 >> bold >> centered <| fromString "Class Load Calculator"

color1 = rgb 27 124 192
color2 = rgb 0 0 0xcd
color3 = rgb 230 238 255

port allCourses : Signal (List (String, (Int, Bool)), List String)

{- Main view functions -}

main : Signal Html
main = fromElement <~ (view <~ Window.dimensions ~ getState)

view : (Int, Int) -> State -> Element
view (w, h) s = flow down [header w headerHeight,
                           mainContainer w h s]

header : Int -> Int -> Element
header w h = color color1 <| container w h middle titleText

explanation : Int -> Element
explanation w = width w <| leftAligned <| Text.concat [fromString "Select the courses you want to take from the left column and get a workload score in the right column. Course workloads are based on the ", link "http://www.eecs.umich.edu/eecs/undergraduate/survey/all_survey.2014.htm" (fromString "EECS workload survey"), fromString "."]

-- Centers the appContainer
mainContainer : Int -> Int -> State -> Element
mainContainer w h s = color color3 (container w (h - headerHeight) midTop
                                                  <| flow down [spacer 1 20,
                                                                explanation 790,
                                                                spacer 1 20,
                                                                appContainer s 250 780,
                                                                spacer 1 20,
                                                                container 790 20 middle <| genPermalink s])

-- Contains the checkboxContainer, selectedCoursesContainer, and resultsContainer
appContainer : State -> Int -> Int -> Element
appContainer s w h =
  let gap = 20
  in flow right [checkboxContainer s.courseInfo w h,
                 spacer gap 1,
                 selectedCoursesContainer s w h,
                 spacer gap 1,
                 resultsContainer s w h]

-- Contains a checkbox for each course
checkboxContainer : Dict.Dict String (Int, Bool) -> Int -> Int -> Element
checkboxContainer courseInfo w h =
    let courses = Dict.foldl (\k (i, b) acc -> acc ++ [(b, k)]) [] courseInfo
    in color color1 <| container w h midTop
           <| flow down
           <| [spacer 1 2] ++ (List.intersperse (spacer 1 2)
                               <| List.map (uncurry <| makeCheckableButton (color3, color1) (color2, color3) (w - 4) 40) courses) ++ [spacer 1 2]

-- Makes checkable button that acts as a checkbox
makeCheckableButton : (Color, Color) -> (Color, Color) -> Int -> Int -> Bool -> String -> Element
makeCheckableButton upColor downColor w h b label =
    let button bColor tColor = color bColor <| container w h middle <| centered <| typeface ["Roboto", "sans-serif"] <| height 22 <| Text.color tColor <| fromString label
        (backgroundColor, textColor) = if b then downColor else upColor
    in Input.clickable (Signal.send click label) <| button backgroundColor textColor

-- Signal sent when a checkable button is checked/unchecked. Stores the name of the course
click : Signal.Channel String
click = Signal.channel ""

-- Contains a list of courses that were selected
selectedCoursesContainer : State -> Int -> Int -> Element
selectedCoursesContainer state w h =
  let selectedCourses = (List.map (\s -> color color3
                                   <| container (w - 4) 40 middle
                                   <| centered
                                   <| Text.typeface ["Roboto", "sans-serif"]
                                   <| height 22
                                   <| Text.color color1
                                   <| fromString s)
                         (Set.toList state.selectedCourses))
  in color color1 <| container w h midTop <| flow down <| [spacer 1 2] ++ (List.intersperse (spacer 1 2) selectedCourses) ++ [spacer 1 2]

-- Returns true if the given course exists in State.selectedCourses but is unchecked according to State.courseInfo
isUnchecked : State -> String -> Bool
isUnchecked state s =
    let (Just (_, b)) = Dict.get s state.courseInfo
    in b == False

-- Contains the final workload score and a description
resultsContainer : State -> Int -> Int -> Element
resultsContainer state w h =
  let (Just sum) = Set.foldl (\s (Just acc) -> Maybe.map (\(i, _) -> i + acc)
                                        (Dict.get s state.courseInfo))
                   (Just 0) state.selectedCourses
  in color color1 <| container w h midTop <| flow down [
          spacer 1 2,
          color color3 <| container (w - 4) 80 middle <| centered <| height 22 <| typeface ["Roboto", "sans-serif"] <| Text.color color1 <| fromString <| "Workload score: " ++ toString sum,
          spacer 1 2,
          color color3 <| container (w - 4) 80 middle <| width (w - 4) <| centered <| height 22 <| typeface ["Roboto", "sans-serif"] <| Text.color color1 <| fromString <| "Semester rating: " ++ resultsMessage sum]

-- Returns a description based on a workload score
resultsMessage : Int -> String
resultsMessage sum = if | sum <= 5 -> "Easy"
                        | sum > 5 && sum <= 10 -> "Not too bad"
                        | sum > 10 && sum <= 15 -> "You'll be busy"
                        | sum > 15 -> "It's going to be a tough semester..."

