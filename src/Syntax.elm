module Syntax exposing (..)

import Element exposing (..)
import Element.Font as Font


invertColors =
    --True
    False


invertGrays =
    --True
    False



--- Code Color Functions --


colorEl : Color -> Element msg -> Element msg
colorEl color elem =
    el [ Font.color color ] <| elem


gray : Element msg -> Element msg
gray =
    colorEl <| invG <| rgb 0.5 0.5 0.5


normal : Element msg -> Element msg
normal =
    colorEl <| invG <| rgb 0.8 0.8 0.8


aqua : Element msg -> Element msg
aqua =
    colorEl <| inv <| rgb 0.2 0.7 0.9


blue : Element msg -> Element msg
blue =
    colorEl <| inv <| rgb 0 0.4 0.8


yellow : Element msg -> Element msg
yellow =
    colorEl <| inv <| rgb 1 1 0.5


orange : Element msg -> Element msg
orange =
    colorEl <| inv <| rgb 0.9 0.6 0.3


white : Element msg -> Element msg
white =
    colorEl <| invG <| rgb 1 1 1


inv : Color -> Color
inv =
    inverted invertColors


invG : Color -> Color
invG =
    inverted invertGrays


inverted : Bool -> Color -> Color
inverted bool color =
    case bool of
        True ->
            let
                rgb =
                    color |> toRgb
            in
            { rgb
                | red = 1.0 - rgb.red
                , green = 1.0 - rgb.green
                , blue = 1.0 - rgb.blue
            }
                |> fromRgb

        False ->
            color
