module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (div)
import Html.Attributes as HAttr 
import TypedSvg.Attributes as SvgAttr exposing (x, y, x1, y1, x2, y2, cx, cy, r, x, width, height, fill)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))
import Url exposing (Url)
import TypedSvg exposing (g, rect, style, svg, line, path)
import TypedSvg exposing (circle)
import TypedSvg.Types exposing (px, Paint(..))
import Color exposing (rgb255)
import TypedSvg.Attributes exposing (transform)
import Shape exposing (Arc)
import Path
import Shape exposing (arc)
import TypedSvg.Attributes exposing (id)
import TypedSvg.Attributes exposing (d)

type alias Model =
    {  key : Nav.Key }


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest

w : Float 
w = 900 

h : Float 
h = 520 




view : Model -> Browser.Document Msg
view _ =
    { title = "counter"
    , body =
        [ div
            [ HAttr.class "mx-5 space-y-20 sm:mx-0 sm:mx-auto lg sm:max-w-xl lg:max-w-3xl mt-10 space-x-10" ]
            [ svg [ SvgAttr.viewBox 0 0 w h ]
                [ style [] []
                , circle [cx (px 100), cy (px 100), r (px 100)] []
                , rect [x (px 250), y (px 0), width (px 100), height (px 200)] [] 
                , circle [cx (px 100), cy (px 350), r (px 100), SvgAttr.fill <| Paint <|  rgb255 250 191 171 ] []
                , rect [x (px 250), y (px 250), width (px 100), height (px 200), SvgAttr.fill <| Paint <| rgb255 250 191 171] [] 
                , g [ SvgAttr.fill <| Paint <| rgb255 42 251 175, SvgAttr.stroke <| Paint <| rgb255 52 83 69
                    ] 
                    [ circle [cx <| px 500, cy <| px 100, r <| px 100, SvgAttr.strokeWidth <| px 2] []
                    , rect [ x <| px 650, y <| px 0, width <| px 100, height <| px 200 ] [] 
                    ] 
                , line [x1 <| px 380, y1 <| px 0, x2 <| px 380, y2 <| px 520, SvgAttr.stroke <| Paint <| Color.red, SvgAttr.strokeWidth <| px 2] []
                , path 
                    [ SvgAttr.stroke <| Paint <| Color.orange
                        , SvgAttr.strokeWidth <| px 3
                        , fill <| PaintNone
                        , SvgAttr.d "M 0,220 L 900,220 L 900,510 L 380, 510  L 900,220"
                     ]
                     []
                , path 
                    [ SvgAttr.stroke <| Paint <| Color.green
                    , SvgAttr.strokeWidth <| px 1 
                    , fill <| Paint <| rgb255 171 90 181
                    , SvgAttr.d "M 900, 220 L 900, 510 L380,510 L 900,220"
                    ]
                    []
                ] 
            , svg 
                [SvgAttr.viewBox 0 0 w h] 
                [ circle 
                    [ r <| px 200
                    , cx <| px  <| w / 2
                    , cy <| px <| h / 2
                    , fill <| Paint <| Color.yellow 
                    , SvgAttr.stroke <| Paint <| Color.black
                    ]  [] 
                    , g  -- eye group 
                        [ fill <| Paint <| Color.black 
                        , transform [Translate (w/2) (h/2)]
                        ]
                        [ circle [ r <| px <| eye_radius, cx <| px <| eye_offset, cy <| px <| -eye_height, fill <| Paint <| Color.black ] [] 
                        , circle [ r <| px <| eye_radius, cx <| px <| -eye_offset, cy <| px <| -eye_height, fill <| Paint <| Color.black ] [] 
                        ] -- end of eye group
                    , g 
                        [ transform [Translate (w/2) (h/2 + mouth_offset)] ]
                        [ Path.element (arc mouth) []
                        ] 
                    , g  -- eyebrow 
                        [ id "eye-brow-group"
                        , transform [Translate (w/2) (h/2)]
                        ]
                        [ path 
                            [ transform [Translate (-eyebrow_x_offset) (-eyebrow_y_offset)]
                            , SvgAttr.d <| Path.toString <| arc eyebrow 
                            ] 
                            [
                            ] 
                        , path 
                            [id "right-eyebrow"
                            , transform [Translate eyebrow_x_offset (-eyebrow_y_offset)]
                            , d <| Path.toString <| arc {innerRadius = 40, outerRadius = 50, startAngle = pi/2 - eyebrow_angle_offset, endAngle = -pi/2 + eyebrow_angle_offset, cornerRadius = 0, padAngle = 0, padRadius = 0 } 
                            ]
                            []
                        ] 
                        

                ]
            ]
        ]
    }

pi : Float 
pi = 3.1415926

eyebrow_angle_offset : Float 
eyebrow_angle_offset = pi / 6 

eyebrow_x_offset : Float 
eyebrow_x_offset = eye_offset 

eyebrow_y_offset : Float 
eyebrow_y_offset = 60 

eyebrow : Arc 
eyebrow =   { innerRadius = 40
            , outerRadius = 50
            , cornerRadius = 0
            , startAngle = (pi/2-eyebrow_angle_offset)
            , endAngle = (-pi/2)+eyebrow_angle_offset 
            , padAngle = 0 
            , padRadius = 0
            }

mouth : Arc 
mouth =     { innerRadius = 130
            , outerRadius = 150
            , cornerRadius = 0
            , startAngle = pi / 2 
            , endAngle = (3 * pi) / 2 
            , padAngle = 0 
            , padRadius = 0
            } 

eye_radius : Float 
eye_radius  = 30 

eye_height : Float 
eye_height = 60 

eye_offset : Float 
eye_offset = 80 

mouth_offset : Float 
mouth_offset = 10 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( {  key = key }, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
