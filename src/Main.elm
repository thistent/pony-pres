module Main exposing (..)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Keyboard as Kb
import Syntax as S exposing (fontSize, inv, invG)


type alias Model =
    { slideName : String
    , gameState : GameModel
    , pressedKeys : List Kb.Key
    , newPressed : List Kb.Key
    }


type Msg
    = GetSlide SlideRef
    | KeyMsg Kb.Msg


type SlideRef
    = Slide GameModel String
    | SlideAlias GameModel String String



-- Types for Game Dynamics --


type Completion
    = Completed
    | Incomplete


type alias GameModel =
    { lives : Int
    , items : Dict String (Element Msg)
    , showHud : Bool
    , introState : ( Bool, Bool )
    , readWritePath : ( Bool, Bool )
    , ambAuthPath : Completion
    , actorModPath : Completion
    , refCapPath : Completion
    }


type GameMsg
    = ShowLives
    | HideLives
    | UpdateLives Int
    | ReadWriteFinish
    | AmbAuthFinish
    | ActorModFinish
    | RefCapFinish
    | NoGameOp


gameUpdate : GameMsg -> GameModel -> GameModel
gameUpdate msg model =
    model



-- Main Function --


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { slideName =
            "title"

      --"crossroad"
      , gameState =
            { lives = 3
            , items = Dict.empty
            , showHud = False
            , introState = ( False, False )
            , readWritePath = ( False, False )
            , ambAuthPath = Incomplete
            , actorModPath = Incomplete
            , refCapPath = Incomplete
            }
      , pressedKeys = []
      , newPressed = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSlide slideRef ->
            case slideRef of
                Slide gameState name ->
                    ( { model
                        | slideName = name
                        , gameState = gameState
                      }
                    , Cmd.none
                    )

                SlideAlias gameState name _ ->
                    ( { model
                        | slideName = name
                        , gameState = gameState
                      }
                    , Cmd.none
                    )

        KeyMsg keyMsg ->
            let
                pressedNew =
                    Kb.update keyMsg model.pressedKeys
            in
            ( { model
                | pressedKeys = pressedNew
                , newPressed = model.pressedKeys
              }
            , Cmd.none
            )


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Sub.map KeyMsg Kb.subscriptions
        ]



-- View --


view : Model -> Browser.Document Msg
view model =
    let
        pageStyle : List (Attribute Msg)
        pageStyle =
            [ getExtFont "Fira Code"
            , Bg.color <| invG <| rgb 0 0 0
            , Font.color <| invG <| rgb 0.8 0.8 0.8
            , Font.size <| round fontSize
            , spacing 30
            , width fill
            , height fill
            ]
    in
    { title = "Type Safety in Pony"
    , body =
        [ Element.layout pageStyle <|
            el (fillSpacePlus [ padding 5 ]) <|
                Maybe.withDefault none <|
                    Dict.get model.slideName <|
                        slides model.gameState
        ]
    }



-- Slide Templates --


slide : GameModel -> Element Msg -> Element Msg
slide gameState content =
    column fillSpace <|
        [ content
        , el [ width fill, padding 30 ] <| hud gameState
        ]


fillSpacePlus : List (Attribute Msg) -> List (Attribute Msg)
fillSpacePlus attrs =
    fillSpace ++ attrs


fillSpace : List (Attribute Msg)
fillSpace =
    [ width fill, height fill ]



--


titleText : Element Msg -> Element Msg
titleText title =
    el
        [ centerX
        , paddingEach
            { edges
                | top = 30
                , bottom = 40
            }
        , Font.size <| round <| fontSize * 2
        , Font.color <| invG <| rgb 1 1 1
        , getExtFont "Fredericka the Great"
        ]
    <|
        title



--


getExtFont : String -> Attribute Msg
getExtFont extFont =
    Font.family
        [ Font.external
            { name = extFont
            , url = "https://fonts.googleapis.com/css?family=" ++ extFont
            }
        ]


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }



---


hud : GameModel -> Element Msg
hud gameState =
    row
        [ width fill, spacing 15, Font.size <| round <| fontSize ]
    <|
        if gameState.showHud then
            [ text "Lives: "
            , row [ spacing 15 ] <|
                if gameState.lives > 0 then
                    List.repeat gameState.lives <|
                        el
                            [ Font.color <| inv <| rgb 1 0.3 0.3
                            , getExtFont "Syncopate"
                            , rotate <| -pi / 2
                            ]
                        <|
                            row [ spacing 2 ] [ text "<", text "3" ]

                else
                    [ el
                        [ Font.color <|
                            invG <|
                                rgb 0.2 0.2 0.2
                        ]
                      <|
                        text "(none)"
                    ]
            , el [ width fill ] none
            , row []
                [ text "Items: "
                , if Dict.isEmpty gameState.items then
                    el
                        [ Font.color <|
                            invG <|
                                rgb 0.2 0.2 0.2
                        ]
                    <|
                        text "(none)"

                  else
                    row [] <| Dict.values gameState.items
                ]
            ]

        else
            [ text " " ]


emptyLine : Element Msg
emptyLine =
    el [] <| text " "


codeList =
    [ row [] [ S.white <| text "actor ", S.blue <| text "Main" ]
    , row []
        [ S.white <| text "  new"
        , S.normal <| text " create("
        , S.aqua <| text "env"
        , S.normal <| text ": "
        , S.blue <| text "Env "
        , S.orange <| text "val"
        , S.normal <| text ") =>"
        ]
    , row []
        [ S.aqua <| text "    env"
        , S.normal <| text ".out.print(\"Hello, World!\")"
        ]
    ]


slides : GameModel -> Dict String (Element Msg)
slides gameState =
    Dict.fromList
        [ ( "title"
          , slide gameState <|
                column
                    fillSpace
                    [ el [ centerX, centerY ] <|
                        titleText <|
                            row []
                                [ text "Type Safety in "
                                , gotoSlide <|
                                    SlideAlias
                                        gameState
                                        "why functional"
                                        "Pony"
                                ]
                    ]
          )
        , ( "why functional"
          , slide gameState <|
                column
                    fillSpace
                    [ el [ centerX, centerY ] <|
                        row [] <|
                            if Dict.member "mutVar" gameState.items then
                                [ gotoSlide <|
                                    SlideAlias gameState
                                        "what pony"
                                        "Functional Programming"
                                , text ", we already know why."
                                ]

                            else
                                [ text "Why "
                                , gotoSlide <|
                                    SlideAlias gameState
                                        "what pony"
                                        "Functional Programming"
                                , text " ?"
                                ]
                    ]
          )
        , ( "what pony"
          , column fillSpace
                [ row [ width fill, height <| fillPortion 7 ]
                    [ el fillSpace <|
                        el [ centerX, centerY ] <|
                            if Tuple.first gameState.introState == False then
                                el
                                    [ Font.size <| round <| fontSize * 2 / 3
                                    , Font.bold
                                    ]
                                <|
                                    gotoSlide <|
                                        SlideAlias
                                            { gameState
                                                | introState =
                                                    ( True
                                                    , Tuple.second
                                                        gameState.introState
                                                    )
                                            }
                                            "what pony"
                                            "About Pony"

                            else
                                column [ spacing 30 ]
                                    [ el
                                        [ Font.size <|
                                            round <|
                                                fontSize
                                                    * 4
                                                    / 3
                                        , Font.bold
                                        ]
                                      <|
                                        text "About Pony :"
                                    , column [ spacing 30 ]
                                        [ text "-> Sylvan Clebsch, 2014"
                                        , text "-> The Actor Model"
                                        , text "-> Object-Oriented Features"
                                        , text "-> Mutable Variables!"
                                        , text "-> LLVM-Based Compiler"
                                        ]
                                    ]
                    , el fillSpace <|
                        el [ centerX, centerY ] <|
                            if Tuple.second gameState.introState == False then
                                el
                                    [ Font.size <| round <| fontSize * 2 / 3
                                    , Font.bold
                                    ]
                                <|
                                    gotoSlide <|
                                        SlideAlias
                                            { gameState
                                                | introState =
                                                    ( Tuple.first
                                                        gameState.introState
                                                    , True
                                                    )
                                            }
                                            "what pony"
                                            "Some Guarantees"

                            else
                                column [ spacing 30 ]
                                    [ el
                                        [ Font.size <|
                                            round <|
                                                fontSize
                                                    * 4
                                                    / 3
                                        , Font.bold
                                        ]
                                      <|
                                        text "Some Guarantees :"
                                    , column [ spacing 30 ]
                                        [ text "-> Type Safety"
                                        , text "-> Memory Safety"
                                        , text "-> No Runtime Exceptions"
                                        , text "-> No Data-Races"
                                        , text "-> Deadlock Free (No Locks)"
                                        , text "-> Native Code (with C FFI)"
                                        ]
                                    ]
                    ]
                , el [ width fill, height <| fillPortion 1 ] <|
                    el [ alignRight, alignBottom ] <|
                        if gameState.introState == ( True, True ) then
                            gotoSlide <|
                                SlideAlias gameState "choose" "->"

                        else
                            el [] none
                ]
          )
        , ( "choose"
          , column fillSpace <|
                [ column [ centerX, centerY, spacing 10 ]
                    [ row []
                        [ text "This is "
                        , if Dict.member "mutVar" gameState.items then
                            el [ Font.bold ] <| text "still "

                          else
                            none
                        , text "a choose your own adventure"
                        ]
                    , el [] <| text "presentation!"
                    ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias { gameState | showHud = True }
                            "mutVar 1"
                            "->"
                ]
          )
        , ( "mutVar 1"
          , slide gameState <|
                row fillSpace
                    [ paragraph
                        [ spacing 10
                        , padding 30
                        , Font.letterSpacing 1.0
                        , Font.size <| round <| fontSize
                        , Font.justify
                        , centerY
                        ]
                      <|
                        if Dict.member "mutVar" gameState.items then
                            [ text "You are in in a block of code. "
                            , text "This is where you found your reference to a "
                            , text "mutable variable."
                            ]

                        else
                            [ text "You are in in a block of code. "
                            , text "You see a reference to a mutable variable. "
                            , text "What would you like to do?"
                            ]
                    , el fillSpace <|
                        if Dict.member "mutVar" gameState.items then
                            el [ alignRight, alignBottom ] <|
                                gotoSlide <|
                                    SlideAlias gameState
                                        "mutVar 2"
                                        "Continue ->"

                        else
                            el [ centerX, centerY ] <|
                                S.yellow <|
                                    gotoSlide <|
                                        SlideAlias
                                            { gameState
                                                | items =
                                                    Dict.insert
                                                        "mutVar"
                                                        (S.yellow <|
                                                            text "mutVar"
                                                        )
                                                        gameState.items
                                            }
                                            "mutVar 1"
                                            "mutVar"
                    ]
          )
        , ( "mutVar 2"
          , slide gameState <|
                column fillSpace
                    [ row
                        [ centerX, centerY ]
                        [ text "Would you like to try to "
                        , if Tuple.first gameState.readWritePath == False then
                            gotoSlide <|
                                SlideAlias
                                    { gameState
                                        | readWritePath =
                                            ( True
                                            , Tuple.second gameState.readWritePath
                                            )
                                    }
                                    "read mutVar"
                                    "read"

                          else
                            el
                                [ Font.strike
                                , Font.color <| inv <| rgb 1 0 0
                                ]
                            <|
                                text "read"
                        , text " or "
                        , if Tuple.second gameState.readWritePath == False then
                            gotoSlide <|
                                SlideAlias
                                    { gameState
                                        | readWritePath =
                                            ( Tuple.first gameState.readWritePath
                                            , True
                                            )
                                    }
                                    "write mutVar"
                                    "write"

                          else
                            el
                                [ Font.strike
                                , Font.color <| inv <| rgb 1 0 0
                                ]
                            <|
                                text "write"
                        , S.yellow <| text " mutVar"
                        , text "?"
                        ]
                    , if gameState.readWritePath == ( True, True ) then
                        column
                            [ alignRight
                            , alignBottom
                            , paddingXY 30 60
                            , spacing 20
                            ]
                            [ text " Maybe you should "
                            , gotoSlide <|
                                SlideAlias
                                    gameState
                                    "rethink 1"
                                    "Rethink your Approach"
                            ]

                      else
                        el [] none
                    ]
          )
        , ( "read mutVar"
          , fatalError
                (if xor (Tuple.first gameState.readWritePath) (Tuple.second gameState.readWritePath) then
                    "title"

                 else
                    "mutVar 2"
                )
                gameState
                [ text "Unfortunately, someone was writing to "
                , S.yellow <| text "mutVar "
                , text "while you were reading. "
                , text "Consuming corrupted data is hazardous "
                , text "to your health."
                ]
          )
        , ( "write mutVar"
          , fatalError
                (if
                    xor
                        (Tuple.first gameState.readWritePath)
                        (Tuple.second gameState.readWritePath)
                 then
                    "title"

                 else
                    "mutVar 2"
                )
                gameState
                [ text "You get hit by a "
                , el [ Font.bold ] <| text "Memory Bus "
                , text "in mid write cycle "
                , text "and die a horrible death!"
                ]
          )
        , ( "rethink 1"
          , slide gameState <|
                column fillSpace
                    [ row (fillSpacePlus [ padding 30, spacing 30 ])
                        [ el fillSpace <|
                            column [ centerX, centerY, spacing 30 ]
                                [ el [ Font.bold ] <|
                                    text "Avoiding corrupted data:"
                                , paragraph []
                                    [ text
                                        "-> Any number of concurrent reads "
                                    , text "are OK"
                                    ]
                                , paragraph []
                                    [ text "-> Only one write at a time is OK"
                                    ]
                                , paragraph []
                                    [ text "-> You cannot read and write "
                                    , text "concurrently"
                                    ]
                                ]
                        , el fillSpace <|
                            row [ centerX, centerY, spacing 30 ]
                                [ S.blue <| text "Read"
                                , text "|"
                                , S.orange <| text "Write"
                                ]
                        ]
                    , el [ alignRight, alignBottom ] <|
                        gotoSlide <|
                            SlideAlias gameState "crossroad" "->"
                    ]
          )
        , ( "crossroad"
          , slide gameState <|
                el fillSpace <|
                    el [ centerX, centerY ] <|
                        column [ spacing 30 ]
                            [ titleText <| text "Where to go now?"
                            , gotoSlide <|
                                SlideAlias gameState
                                    "refCaps 1"
                                    "Reference Capabilities"
                            , gotoSlide <|
                                SlideAlias gameState
                                    "objCaps 1"
                                    "Object Capabilities"
                            , gotoSlide <|
                                SlideAlias gameState "subTypes 1" "Sub-Typing"
                            ]
          )
        , ( "refCaps 1"
          , column fillSpace
                [ row (fillSpacePlus [ spacing 30, padding 30 ])
                    [ el fillSpace <|
                        column [ centerX, centerY, spacing 30 ]
                            [ el [ Font.bold ] <|
                                text "Some Pony Specifics:"
                            , paragraph []
                                [ text "-> Code inside an actor "
                                , text "is sequential"
                                ]
                            , paragraph []
                                [ text
                                    "-> Different actors can run "
                                , text "concurrently"
                                ]
                            , paragraph [ Font.justify ]
                                [ text
                                    "-> Actors are the same as "
                                , text "objects, except that they can have "
                                , text "asynchronous methods"
                                ]
                            ]
                    , el fillSpace <|
                        row [ centerX, centerY, spacing 30 ]
                            [ S.blue <| text "Read"
                            , text "|"
                            , S.orange <| text "Write"
                            ]
                    ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "refCaps 2" "->"
                ]
          )
        , ( "refCaps 2"
          , column fillSpace
                [ el [ centerX, centerY, Font.size <| round <| fontSize * 1.4 ] <|
                    text "iso -> trn -> ( ref | val ) -> box -> tag"
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "refCaps 3" "->"
                ]
          )
        , ( "refCaps 3"
          , column fillSpace
                [ el fillSpace <|
                    el [ centerX, centerY, Font.size <| round <| fontSize * 1.4 ] <|
                        text "iso -> trn -> ( ref | val ) -> box -> tag"
                , el (fillSpacePlus [ Font.size <| round <| fontSize ]) <|
                    column [ centerX, spacing 30 ]
                        [ row []
                            [ el [ Font.bold ] <| text "Mutable: "
                            , S.orange <| text "iso"
                            , text ", "
                            , S.orange <| text "ref"
                            , text ", "
                            , S.orange <| text "trn"
                            ]
                        , row []
                            [ el [ Font.bold ] <| text "Immutable: "
                            , S.orange <| text "val"
                            , text ", "
                            , S.orange <| text "box"
                            ]
                        , row []
                            [ el [ Font.bold ] <| text "Opaque: "
                            , S.orange <| text "tag"
                            ]
                        ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "refCaps 4" "->"
                ]
          )
        , ( "refCaps 4"
          , column fillSpace
                [ el fillSpace <|
                    el [ centerX, centerY, Font.size <| round <| fontSize * 1.4 ] <|
                        text "iso -> trn -> ( ref | val ) -> box -> tag"
                , row fillSpace
                    [ el (fillSpacePlus [ Font.size <| round <| fontSize ]) <|
                        column [ centerX, spacing 30 ]
                            [ row []
                                [ el [ Font.bold ] <| text "Mutable: "
                                , S.orange <| text "iso"
                                , text ", "
                                , S.orange <| text "ref"
                                , text ", "
                                , S.orange <| text "trn"
                                ]
                            , row []
                                [ el [ Font.bold ] <| text "Immutable: "
                                , S.orange <| text "val"
                                , text ", "
                                , S.orange <| text "box"
                                ]
                            , row []
                                [ el [ Font.bold ] <| text "Opaque: "
                                , S.orange <| text "tag"
                                ]
                            ]
                    , el fillSpace <|
                        column [ centerX, spacing 30 ] <|
                            [ row []
                                [ el [ Font.bold ] <| text "Readable: "
                                , S.orange <| text "ref"
                                , text ", "
                                , S.orange <| text "val"
                                , text ", "
                                , S.orange <| text "box"
                                ]
                            , row []
                                [ el [ Font.bold ] <| text "Sendable: "
                                , S.orange <| text "iso"
                                , text ", "
                                , S.orange <| text "val"
                                , text ", "
                                , S.orange <| text "tag"
                                ]
                            , row []
                                [ el [ Font.bold ] <| text "Sharable: "
                                , S.orange <| text "val"
                                , text ", "
                                , S.orange <| text "tag"
                                ]
                            , row []
                                [ el [ Font.bold ] <| text "Aliases: "
                                , S.orange <| text "ref"
                                , text ", "
                                , S.orange <| text "val"
                                , text ", "
                                , S.orange <| text "box"
                                , text ", "
                                , S.orange <| text "tag"
                                ]
                            ]
                    ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "crossroad" "->"
                ]
          )
        , ( "objCaps 1"
          , column (fillSpacePlus [ spacing 30 ])
                [ el fillSpace <|
                    el [ centerX, alignBottom, Font.bold ] <|
                        text "Object Capabilities in Pony"
                , el fillSpace <|
                    column [ centerX, spacing 20 ]
                        [ text "An unforgable token identifies each object. "
                        , text "It give a program authority to do things "
                        , text "with that object."
                        ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "objCaps 2" "->"
                ]
          )
        , ( "objCaps 2"
          , column (fillSpacePlus [ spacing 39 ])
                [ el fillSpace <|
                    el [ centerX, alignBottom, Font.bold ] <|
                        text "Object Capabilities"
                , el fillSpace <|
                    column [ centerX, spacing 40 ]
                        [ row []
                            [ text "-> Don't separate "
                            , S.aqua <| text "Designation"
                            , text " from "
                            , S.aqua <| text "Authority"
                            , text "."
                            ]
                        , column [ spacing 10 ]
                            [ row []
                                [ text "-> No "
                                , S.yellow <| text "Ambient Authority"
                                , text " like global variables"
                                ]
                            , text "   or unrestricted access to the file system."
                            ]
                        , text "-> No pointer arithmetic."
                        , column [ spacing 10 ]
                            [ text "-> Trust Boundaries. You may need to trust"
                            , text "   C code to get stuff done, but you can"
                            , text "   restrict the compiler to use only approved"
                            , text "   non-Pony libraries."
                            ]
                        ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "crossroad" "->"
                ]
          )
        , ( "subTypes 1"
          , column (fillSpacePlus [ spacing 30 ])
                [ el fillSpace <|
                    el [ centerX, alignBottom, Font.bold ] <|
                        text "Types in Pony"
                , el fillSpace <|
                    column [ centerX, spacing 20 ]
                        [ text "Types in Pony are similar to those in other "
                        , text "functional languages, but with some added "
                        , text "qualities related to Sub-Typing."
                        ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "subTypes 2" "->"
                ]
          )
        , ( "subTypes 2"
          , column (fillSpacePlus [ spacing 39 ])
                [ el fillSpace <|
                    el [ centerX, alignBottom, Font.bold ] <|
                        text "Sub-Types in Pony"
                , el fillSpace <|
                    column [ centerX, spacing 40 ]
                        [ row []
                            [ text "-> Pony has both "
                            , column [ Font.center ]
                                [ el [ width fill, Font.bold ] <|
                                    text "Traits"
                                , S.gray <| text "(Nominal)"
                                ]
                            , text " and "
                            , column [ Font.center ]
                                [ el [ width fill, Font.bold ] <|
                                    text "Interfaces"
                                , S.gray <| text "(Structural)"
                                ]
                            , text " sub-typing."
                            ]
                        , column [ spacing 10 ]
                            [ row []
                                [ text "-> A sub-type contains a super-set of"
                                , text " the traits"
                                ]
                            , text "   it inherits."
                            ]
                        , text "-> Primitive Types | Union Types | Intersection Types"
                        ]
                , el [ alignRight, alignBottom ] <|
                    gotoSlide <|
                        SlideAlias gameState "crossroad" "->"
                ]
          )
        ]


fatalError : String -> GameModel -> List (Element Msg) -> Element Msg
fatalError ref gameState msg =
    row (fillSpacePlus [ Bg.color <| inv <| rgb 0 0 0.6 ]) <|
        [ el [ width <| fillPortion 1 ] none
        , column
            [ centerX
            , centerY
            , padding 10
            , spacing 40
            , width <| fillPortion 2
            ]
            [ el
                [ centerX
                , Bg.color <| inv <| rgb 0.7 0.7 0.7
                , Font.color <| inv <| rgb 0 0 0.6
                ]
              <|
                gotoSlide <|
                    SlideAlias
                        { gameState | lives = gameState.lives - 1 }
                        ref
                        "FATAL ERROR"
            , paragraph
                [ centerX
                , Bg.color <| inv <| rgb 0 0 0.6
                , Font.color <| inv <| rgb 0.7 0.7 0.7

                --, Font.justify
                , spacing 10
                ]
                msg
            ]
        , el [ width <| fillPortion 1 ] none
        ]


gotoSlide : SlideRef -> Element Msg
gotoSlide slideRef =
    let
        ( lifeDelta, slideName, caption ) =
            case slideRef of
                Slide l n ->
                    ( l, n, n )

                SlideAlias l n c ->
                    ( l, n, c )
    in
    Input.button
        []
        { onPress = Just <| GetSlide slideRef
        , label =
            el
                [ Border.width 1
                , Border.color <| invG <| rgb 0.3 0.3 0.3
                , Border.dashed
                , Border.rounded 10
                , paddingEach
                    { edges
                        | top = 10
                        , bottom = 15
                        , left = 15
                        , right = 15
                    }
                ]
            <|
                text caption
        }