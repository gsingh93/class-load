import Color (..)
import Debug
import Easing
import Easing (ease, easeInQuad)
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
import Text (plainText, fromString, centered, height, bold, asText, typeface)
import Time
import Time (Time, second, fps)
import Signal
import Signal (Signal, (<~), (~), foldp)

{-
The view updates based on the following signals:
 - A window resize
 - A state change
 - Checking/unchecking a checkbox
 - 60 times a second

A state change occurs when a checkbox is checked/unchecked
 - getState gets the name of the course from the signal, flips the "checked" Bool in State.courseInfo, and adds/removes the course name from State.selectedCourses

Animations are currently disabled in this branch.
-}

{---------- State functions ---------}

type alias State = {
  -- A mapping of course name to (credits, checked) tuple
  courseInfo : Dict.Dict String (Int, Bool),
  -- A Set of the names of the selected courses
  selectedCourses : Set.Set String}

-- Creates the initial dictionary for State.courseInfo
makeInitialDict : Dict.Dict String (Int, Bool)
makeInitialDict = List.foldl (\(k, v) d -> Dict.insert k (v, False) d)
           Dict.empty allCourses

-- Update the state when a checkbox is clicked
updateState : String -> State -> State
updateState s state =
  let newCourseInfo = Dict.update s (\(Just (credits, b)) -> Just (credits, not b))
                      state.courseInfo
      (Just (_, b)) = Dict.get s state.courseInfo
  in case not b of
      True -> { courseInfo = newCourseInfo,
                selectedCourses = Set.insert s state.selectedCourses}
      False -> { courseInfo = newCourseInfo,
                 selectedCourses = Set.remove s state.selectedCourses}

getState : Signal State
getState = foldp updateState { courseInfo = makeInitialDict,
                               selectedCourses = Set.empty} (Signal.subscribe click)

{---------- Util functions and constants ----------}

zip : List a -> List b -> List (a,b)
zip listX listY =
  case (listX, listY) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    (  _  ,   _  ) -> []

port title : String
port title = "Course Load Calculator"

headerHeight : Int
headerHeight = 100

titleText : Element
titleText = typeface ["sans-serif"] >> Text.color color3 >> height 40 >> bold >> centered <| fromString "Class Load Calculator"

color1 = rgb 27 124 192
color2 = rgb 0 0 0xcd
color3 = rgb 230 238 255

allCourses : List (String, Int)
allCourses = [("EECS 280", 3), ("EECS 281", 5), ("EECS 482", 5),
              ("EECS 483", 4)]

{- Main view functions -}

main : Signal Element
main = view <~ Window.dimensions ~ getState ~ (fst <~ (Time.timestamp <| Signal.subscribe click)) ~ (fst <~ (Time.timestamp <| fps 60))

view : (Int, Int) -> State -> Time -> Time -> Element
view (w, h) s initTime curTime = flow down [header w headerHeight,
                                            mainContainer w h initTime curTime s]

header : Int -> Int -> Element
header w h = color color1 <| container w h middle titleText

-- Centers the appContainer
mainContainer : Int -> Int -> Time -> Time -> State -> Element
mainContainer w h initTime curTime s = color color3 (container w (h - headerHeight) midTop
                                                     <| flow down [spacer 1 20,
                                                                   appContainer s initTime curTime 250 600])

-- Contains the checkboxContainer, selectedCoursesContainer, and resultsContainer
appContainer : State -> Time -> Time -> Int -> Int -> Element
appContainer s initTime curTime w h =
  let gap = 20
  in flow right [checkboxContainer s.courseInfo w h,
                 spacer gap 1,
                 selectedCoursesContainer s initTime curTime w h,
                 spacer gap 1,
                 resultsContainer s w h]

-- Contains a checkbox for each course
checkboxContainer : Dict.Dict String (Int, Bool) -> Int -> Int -> Element
checkboxContainer courseInfo w h =
    let courses = zip (snd <| List.unzip <| Dict.values courseInfo)
                  (Dict.keys courseInfo)
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
selectedCoursesContainer : State -> Time -> Time -> Int -> Int -> Element
selectedCoursesContainer state initTime curTime w h =
  let selectedCourses = (List.map (\s -> makeFadeableElement (isUnchecked state s)
                                         initTime curTime (w - 4, 40)
                                         (\w h b f -> color b
                                          <| container w h middle
                                          <| centered
                                          <| Text.typeface ["Roboto", "sans-serif"]
                                          <| height 22
                                          <| Text.color f
                                          <| fromString s))
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
          color color3 <| container (w - 4) 80 middle <| centered <| height 22 <| Text.color color1 <| fromString <| "Workload score: " ++ toString sum,
          spacer 1 2,
          color color3 <| container (w - 4) 80 middle <| width (w - 4) <| centered <| height 22 <| Text.color color1 <| fromString <| "Semester rating: " ++ resultsMessage sum]

-- Returns a description based on a workload score
resultsMessage : Int -> String
resultsMessage sum = if | sum <= 5 -> "Easy"
                        | sum > 5 && sum <= 10 -> "Not too bad"
                        | sum > 10 && sum <= 15 -> "You'll be busy"
                        | sum > 15 -> "It's going to be a tough semester..."

{- Animation functions -}

makeFadeableElement : Bool -> Time -> Time -> (Int, Int) -> (Int -> Int -> Color -> Color -> Element) -> Element
makeFadeableElement doAnim initialTime curTime (w, h) makeElement =
    let animColor = if doAnim
                    then (animation (fst start) (fst end) (curTime - initialTime),
                          animation (snd start) (snd end) (curTime - initialTime))
                    else start
        start = (color3, color1)
        end = (setAlpha 0 <| fst start, setAlpha 0 <| snd start)
    in uncurry (makeElement w h) animColor

setAlpha : Float -> Color -> Color
setAlpha a c =
    let color = toRgb c
    in rgba color.red color.green color.blue a

animation : Color -> Color -> Time -> Color
animation start end = ease easeInQuad Easing.color start end <| 0.3 * second

{- Unused:
check : Signal.Channel (String, Bool)
check = Signal.channel ("", False)

createCheckbox : Bool -> String -> Element
createCheckbox checked label =
  flow right [container 30 30 middle
              (Input.checkbox (\b -> Signal.send check (label, b)) checked),
              container 100 30 middle <| centered <| Text.color color3 <| fromString label]
-}
