module Main exposing (..)

import Browser
import Browser.Dom
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg
import TypedSvg.Types as SA
import Zoom


type alias Transform =
    { translate : { x : Float, y : Float }, scale : Float }


type alias LatLng =
    { lat : Float, lng : Float }


type alias Viewport =
    { x : Float, y : Float, width : Float, height : Float }


type alias Model =
    { zoomAndPan : Zoom.Zoom
    , latLng : LatLng
    , zoomLevel : Int
    , viewport : Viewport
    , almost2 : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initViewport =
            { x = 0, y = 0, width = 0, height = 0 }
    in
    ( { zoomAndPan = initZoomAndPan { viewport = initViewport }
      , latLng = { lat = 0, lng = 0 }
      , zoomLevel = 2
      , viewport = initViewport
      , almost2 = almost 2
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


initZoomAndPan : { b | viewport : { a | height : Float, width : Float } } -> Zoom.Zoom
initZoomAndPan model =
    Zoom.init { width = model.viewport.width, height = model.viewport.height }


type Msg
    = OnZoom Zoom.OnZoom
    | GotViewport Browser.Dom.Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport browserDomViewport ->
            ( { model
                | viewport = browserDomViewport.viewport
                , zoomAndPan = initZoomAndPan model
              }
            , Cmd.none
            )

        OnZoom zoomMsg ->
            let
                newZoomAndPan =
                    Zoom.update zoomMsg model.zoomAndPan
                        |> Zoom.scaleExtent 0.99 4

                newZoomAndPanAsRecord =
                    Zoom.asRecord newZoomAndPan
            in
            if newZoomAndPanAsRecord.scale >= 2 && model.zoomLevel < 19 then
                --
                -- Zooming IN
                --
                ( { model
                    | zoomLevel = model.zoomLevel + 1
                    , latLng =
                        getLatLngCenter
                            { viewport = model.viewport
                            , zoomAndPanAsRecord = Zoom.asRecord model.zoomAndPan
                            , zoomLevel = model.zoomLevel
                            , latLng = model.latLng
                            }
                    , zoomAndPan =
                        initZoomAndPan model
                            |> Zoom.setTransform
                                Zoom.instantly
                                { scale = 1
                                , translate =
                                    { x = 0
                                    , y = 0
                                    }
                                }
                  }
                , Cmd.none
                )

            else if newZoomAndPanAsRecord.scale < 1 && model.zoomLevel > 1 then
                --
                -- Zooming OUT
                --
                ( { model
                    | zoomLevel = model.zoomLevel - 1
                    , latLng =
                        getLatLngCenter
                            { viewport = model.viewport
                            , zoomAndPanAsRecord = Zoom.asRecord model.zoomAndPan
                            , zoomLevel = model.zoomLevel
                            , latLng = model.latLng
                            }
                    , zoomAndPan =
                        initZoomAndPan model
                            |> Zoom.setTransform
                                Zoom.instantly
                                { scale = model.almost2
                                , translate =
                                    -- zooming out, we need to center because
                                    -- it is double than the viewport
                                    { x = -(model.viewport.width / 2)
                                    , y = -(model.viewport.height / 2)
                                    }
                                }
                  }
                , Cmd.none
                )

            else
                ( { model | zoomAndPan = newZoomAndPan }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        latLngCenter =
            getLatLngCenter
                { viewport = model.viewport
                , zoomAndPanAsRecord = Zoom.asRecord model.zoomAndPan
                , zoomLevel = model.zoomLevel
                , latLng = model.latLng
                }

        mapSize =
            toFloat <| 2 ^ model.zoomLevel * 256

        txtyCentertile =
            { tx = lngToTileX latLngCenter.lng model.zoomLevel
            , ty = latToTileY latLngCenter.lat model.zoomLevel
            }

        latLngCornerOfCentertile =
            { lat = tileYTolat txtyCentertile.ty model.zoomLevel
            , lng = tileXTolng txtyCentertile.tx model.zoomLevel
            }

        bxbyWhenEnteredThisZoomInterval =
            { bx = lngToX mapSize model.latLng.lng
            , by = latToY mapSize model.latLng.lat
            }

        viewportStaticCenter =
            { x = model.viewport.width / 2
            , y = model.viewport.height / 2
            }

        xyCornerOfCentertile =
            { x = lngToX mapSize latLngCornerOfCentertile.lng - bxbyWhenEnteredThisZoomInterval.bx + viewportStaticCenter.x
            , y = latToY mapSize latLngCornerOfCentertile.lat - bxbyWhenEnteredThisZoomInterval.by + viewportStaticCenter.y
            }

        maxTiles =
            2 ^ model.zoomLevel

        tilesQty =
            { x = round (model.viewport.width / 256 / 2) + 1
            , y = round (model.viewport.height / 256 / 2) + 1
            }
    in
    div []
        [ Svg.svg [ SA.width <| SA.px model.viewport.width, SA.height <| SA.px model.viewport.height ]
            [ Svg.rect
                ([ SA.width <| SA.px model.viewport.width
                 , SA.height <| SA.px model.viewport.height
                 , SA.fill <| SA.Paint <| Color.rgb255 170 211 222
                 ]
                    ++ Zoom.events model.zoomAndPan OnZoom
                )
                []
            , Svg.g [ Zoom.transform model.zoomAndPan, SA.style "pointer-events: none" ]
                (List.map
                    (\( x, y ) -> viewTile xyCornerOfCentertile model.zoomLevel maxTiles txtyCentertile x y)
                    (listTouples { xFrom = -tilesQty.y, xTo = tilesQty.y, yFrom = -tilesQty.x, yTo = tilesQty.x })
                )
            , viewLatLngTip model.viewport model.zoomLevel latLngCenter
            ]
        , div [ class "header" ] [ h1 [] [ viewElmLogo, text " elm-map" ] ]
        , a [ target "_blank", class "map-footer", href "https://www.openstreetmap.org/copyright" ] [ text "© OpenStreetMap contributors" ]
        , div [ class "map-footer", style "left" "0px" ]
            [ a [ target "_blank", href "https://dev.to/lucamug" ] [ text "dev.to" ]
            , text " "
            , a [ href "https://elm-map.guupa.com/debug.html" ] [ text "Debug" ]
            , text " "
            , a [ href "https://elm-map.guupa.com/" ] [ text "NoDebug" ]
            ]
        ]


primaryColor : Color.Color
primaryColor =
    Color.rgb255 30 128 180


viewElmLogo : Html Msg
viewElmLogo =
    Svg.svg [ SA.viewBox -280 -280 560 560, SA.class [ "elm-logo" ], SA.fill <| SA.Paint <| Color.rgb255 30 128 180 ]
        [ Svg.g [ SA.transform [ SA.Scale 1 -1 ] ]
            [ Svg.polygon [ SA.points [ ( -280, -90 ), ( 0, 190 ), ( 280, -90 ) ], SA.transform [ SA.Translate 0 -210, SA.Rotate 0 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -280, -90 ), ( 0, 190 ), ( 280, -90 ) ], SA.transform [ SA.Translate -210 0, SA.Rotate -90 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -198, -66 ), ( 0, 132 ), ( 198, -66 ) ], SA.transform [ SA.Translate 207 207, SA.Rotate -45 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -130, 0 ), ( 0, -130 ), ( 130, 0 ), ( 0, 130 ) ], SA.transform [ SA.Translate 150 0, SA.Rotate 0 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -191, 61 ), ( 69, 61 ), ( 191, -61 ), ( -69, -61 ) ], SA.transform [ SA.Translate -89 239, SA.Rotate 0 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -130, -44 ), ( 0, 86 ), ( 130, -44 ) ], SA.transform [ SA.Translate 0 106, SA.Rotate -180 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -130, -44 ), ( 0, 86 ), ( 130, -44 ) ], SA.transform [ SA.Translate 256 -150, SA.Rotate -270 0 0 ] ] []
            ]
        ]


viewLatLngTip : Viewport -> Int -> LatLng -> Svg.Svg msg
viewLatLngTip viewport zoomLevel latLngCenter =
    let
        latLng =
            limitDecimal zoomLevel latLngCenter.lat ++ ", " ++ limitDecimal zoomLevel (normalize -180 180 latLngCenter.lng)

        calloutWidth =
            (toFloat (String.length latLng) ^ 1.07) * 6.5 - 20
    in
    Svg.g
        [ SA.style "pointer-events: none"
        , SA.transform [ SA.Translate (viewport.width / 2) (viewport.height / 2), SA.Scale 1 1 ]
        ]
        [ Svg.path
            -- From https://www.aleksandrhovhannisyan.com/blog/svg-tutorial-how-to-code-svg-icons-by-hand/
            [ SA.fill <| SA.Paint <| Color.rgba 1 1 1 0.6
            , SA.strokeWidth <| SA.px 1
            , SA.stroke <| SA.Paint primaryColor
            , SA.d ("""M 0 0 l 10 -10 l """ ++ String.fromFloat calloutWidth ++ """ 0 a 8 8 0 0 0 8 -8 l 0 -9 a 8 8 0 0 0 -8 -8 l -""" ++ String.fromFloat (calloutWidth + 20) ++ """ 0 a 8 8 0 0 0 -8 8 l 0 9 a 8 8 0 0 0 8 8 l 10 0 z""")
            ]
            []
        , Svg.text_
            [ SA.transform [ SA.Translate -12 -18, SA.Scale 1 1 ]
            , SA.fill <| SA.Paint primaryColor
            , SA.style "font-size: 16px"
            , SA.style "font-family: monospace"
            ]
            [ Svg.text latLng ]
        ]


viewTile : { x : Float, y : Float } -> Int -> Int -> { tx : Int, ty : Int } -> Int -> Int -> Svg.Svg msg
viewTile xyCornerOfCentertile zoomLevel maxTiles txtyCentertile dx dy =
    let
        tileHttp zoom tileX tileY =
            "https://tile.openstreetmap.org/"
                ++ String.fromInt zoom
                ++ "/"
                ++ String.fromInt tileX
                ++ "/"
                ++ String.fromInt tileY
                ++ ".png"
    in
    Svg.image
        [ SA.href <|
            tileHttp zoomLevel
                (floor (normalize 0 (toFloat maxTiles) (toFloat (txtyCentertile.tx + dx))))
                (floor (normalize 0 (toFloat maxTiles) (toFloat (txtyCentertile.ty + dy))))
        , SA.width <| SA.px 256
        , SA.height <| SA.px 256
        , SA.x <| SA.px (xyCornerOfCentertile.x + toFloat (256 * dx))
        , SA.y <| SA.px (xyCornerOfCentertile.y + toFloat (256 * dy))
        ]
        []



-- HELPERS


listTouples : { xFrom : Int, xTo : Int, yFrom : Int, yTo : Int } -> List ( Int, Int )
listTouples { xFrom, yFrom, xTo, yTo } =
    List.concat <|
        List.map
            (\x_ ->
                List.map
                    (\y_ ->
                        ( x_, y_ )
                    )
                    (List.range yFrom yTo)
            )
            (List.range xFrom xTo)


almost : Float -> Float
almost base =
    -- Return the largest number smaller than "base"
    let
        epsilon b n =
            let
                n2 : Float
                n2 =
                    n / 2
            in
            if b - n2 == b then
                b - n

            else
                epsilon b n2
    in
    epsilon base 1


normalize : Float -> Float -> Float -> Float
normalize min max value =
    if value >= max then
        normalize min max (value - (max - min))

    else if value < min then
        normalize min max (value + (max - min))

    else
        value


limitDecimal : Int -> Float -> String
limitDecimal zoom float =
    let
        decimalsQty =
            round (toFloat zoom / 5) + 1
    in
    float
        |> String.fromFloat
        |> String.split "."
        |> (\list ->
                case list of
                    [ a, b ] ->
                        String.join "." [ a, String.left decimalsQty b ]

                    _ ->
                        String.join "." list
           )



-- MATH


ln : Float -> Float
ln =
    logBase e


exp : Float -> Float
exp =
    (^) e



-- X,Y MATH


xToLng : Float -> Float -> Float
xToLng mapSize xValue =
    ((360 * xValue) / mapSize) - 180


yToLat : Float -> Float -> Float
yToLat mapSize yValue =
    90 * (-1 + (4 * atan (exp (pi - (2 * pi * yValue) / mapSize))) / pi)


lngToX : Float -> Float -> Float
lngToX mapSize lng =
    (lng + 180) * (mapSize / 360)


latToY : Float -> Float -> Float
latToY mapSize lat =
    (mapSize / 2) - (mapSize * ln (tan ((pi / 4) + ((lat * pi / 180) / 2))) / (2 * pi))



-- TILE MATH


lngToTileX : Float -> Int -> Int
lngToTileX lon z =
    floor ((lon + 180.0) / 360.0 * (2 ^ toFloat z))


latToTileY : Float -> Int -> Int
latToTileY lat z =
    let
        n =
            lat * pi / 180
    in
    floor ((1.0 - ln (tan n + 1.0 / cos n) / pi) / 2 * (2 ^ toFloat z))


tileXTolng : Int -> Int -> Float
tileXTolng x z =
    toFloat x / (2 ^ toFloat z) * 360.0 - 180


tileYTolat : Int -> Int -> Float
tileYTolat y z =
    let
        n =
            pi - 2 * pi * toFloat y / (2 ^ toFloat z)
    in
    180.0 / pi * atan (0.5 * (exp n - exp -n))



-- LAT/LNG MAP CENTER


getLatLngCenter :
    { viewport : Viewport
    , zoomAndPanAsRecord : Transform
    , zoomLevel : Int
    , latLng : LatLng
    }
    -> LatLng
getLatLngCenter { viewport, zoomAndPanAsRecord, zoomLevel, latLng } =
    let
        mapSize =
            toFloat <| 2 ^ zoomLevel * 256

        viewportStaticCenter =
            -- The center of the viewport
            { x = viewport.width / 2
            , y = viewport.height / 2
            }

        viewportDynamicCenter =
            -- The center of the viewport that take in consideration the user
            -- moving around
            { x = (viewportStaticCenter.x - zoomAndPanAsRecord.translate.x) / zoomAndPanAsRecord.scale
            , y = (viewportStaticCenter.y - zoomAndPanAsRecord.translate.y) / zoomAndPanAsRecord.scale
            }

        bxbyWhenEnteredThisZoomInterval =
            { x = lngToX mapSize latLng.lng
            , y = latToY mapSize latLng.lat
            }

        deltas =
            { dx = viewportStaticCenter.x - viewportDynamicCenter.x
            , dy = viewportStaticCenter.y - viewportDynamicCenter.y
            }

        bxbyCenter =
            { bx = bxbyWhenEnteredThisZoomInterval.x - deltas.dx
            , by = bxbyWhenEnteredThisZoomInterval.y - deltas.dy
            }

        latLngCenter =
            { lat = yToLat mapSize bxbyCenter.by
            , lng = xToLng mapSize bxbyCenter.bx
            }
    in
    latLngCenter



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Zoom.subscriptions model.zoomAndPan OnZoom
        }
