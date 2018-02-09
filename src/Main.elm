module Main exposing (..)

import Dict exposing (Dict)
import Task
import Window
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Color
import Collage as C exposing (collage)
import Element
import Text
import Json.Decode as Json


eSize : Int
eSize =
    4


fSize : Int
fSize =
    5


eOffset : Int
eOffset =
    2 ^ (eSize - 1) - 1


main : Program Never Model Msg
main =
    H.program
        { init = ( initModel, Task.perform Resize Window.size )
        , view = view
        , update = \msg m -> ( update msg m, Cmd.none )
        , subscriptions = subscriptions
        }


type alias Model =
    { fp : FP
    , wsize : Window.Size
    , edgeEx8 : Int
    , roundFrom : RoundFrom
    }


initModel : Model
initModel =
    { fp = fpZero
    , wsize = { width = 400, height = 400 }
    , edgeEx8 = edgeEx8min
    , roundFrom = initRoundFrom
    }


type alias RoundFrom =
    { numer : Maybe Int, denom : Maybe Int, roundMode : Maybe () }


initRoundFrom : RoundFrom
initRoundFrom =
    { numer = Nothing, denom = Nothing, roundMode = Nothing }


edgeEx8min : Int
edgeEx8min =
    8 * (1 - eOffset)


edgeEx8max : Int
edgeEx8max =
    8 * (2 ^ eSize - eOffset - 1)


edgeEx8forE : Int -> Int
edgeEx8forE e =
    8 * (e)


type alias FP =
    { s : Bit
    , eee : Dict Int Bit
    , fff : Dict Int Bit
    }


type alias Bit =
    Int


signB2M : Bit -> Int
signB2M b =
    1 - 2 * b


signB2S : Bit -> String
signB2S b =
    if b == 0 then
        "+"
    else
        "-"


bits2Int : Dict Int Bit -> Int
bits2Int bits =
    List.sum (List.map (\( i, b ) -> b * 2 ^ i) <| Dict.toList bits)


decodeFP : FP -> ( Int, Int, Int, Float )
decodeFP fp =
    let
        s =
            signB2M fp.s

        eee =
            bits2Int fp.eee

        ( e, lead ) =
            if eee == 0 then
                ( 1 - eOffset, 0 )
            else
                ( eee - eOffset, 1 )

        m =
            (toFloat lead) + (toFloat (bits2Int fp.fff) / 2 ^ (toFloat fSize))
    in
        ( s, e, lead, m )


evalFP : FP -> Float
evalFP fp =
    let
        ( s, e, lead, m ) =
            decodeFP fp
    in
        (toFloat (s * (2 ^ e))) * m


toBinary : Int -> Int -> Dict Int Bit
toBinary size n =
    let
        getBits size i n =
            if size > 0 then
                Dict.insert i (n % 2) <| getBits (size - 1) (i + 1) (n // 2)
            else
                Dict.empty
    in
        if n < 0 then
            Dict.empty
        else
            getBits size 0 n


rat2FP : Int -> Int -> FP
rat2FP numer denom =
    let
        numerA =
            abs numer

        denomA =
            abs denom

        x =
            (toFloat numer) / (toFloat denom)

        s =
            if x < 0 then
                1
            else
                0

        ( e, isDenorm, isOverflow ) =
            if x == 0 then
                ( 0, True, False )
            else
                let
                    e =
                        floor <| logBase 2 (abs x)
                in
                    ( e, e <= -eOffset, e > eOffset )

        fE =
            fSize - e
    in
        if isDenorm then
            { s = s, eee = eee000, fff = toBinary fSize ((numerA * 2 ^ (fSize + eOffset - 1)) // denomA) }
        else if isOverflow then
            { s = s, eee = eee111, fff = fff000 }
        else if fE > 0 then
            { s = s, eee = toBinary eSize (e + eOffset), fff = toBinary fSize (((numerA * 2 ^ fE) // denomA) - 2 ^ (fSize)) }
        else
            { s = s, eee = toBinary eSize (e + eOffset), fff = toBinary fSize ((numerA // (denomA * 2 ^ (-fE))) - 2 ^ (fSize)) }


eee000 : Dict Int Bit
eee000 =
    Dict.fromList <| List.map (\i -> ( i, 0 )) <| List.range 0 (eSize - 1)


eee111 : Dict Int Bit
eee111 =
    Dict.fromList <| List.map (\i -> ( i, 1 )) <| List.range 0 (eSize - 1)


fff000 : Dict Int Bit
fff000 =
    Dict.fromList <| List.map (\i -> ( i, 0 )) <| List.range 0 (fSize - 1)


fff111 : Dict Int Bit
fff111 =
    Dict.fromList <| List.map (\i -> ( i, 1 )) <| List.range 0 (fSize - 1)


fpZero : FP
fpZero =
    { s = 0, eee = eee000, fff = fff000 }


fpMaxDenorm : FP
fpMaxDenorm =
    { s = 0, eee = eee000, fff = fff111 }


fpMinNorm : FP
fpMinNorm =
    { s = 0, eee = Dict.insert 0 1 eee000, fff = fff000 }


fpMax : FP
fpMax =
    { s = 0, eee = Dict.insert 0 0 eee111, fff = fff111 }


fpInfinity : FP
fpInfinity =
    { s = 0, eee = eee111, fff = fff000 }


fpQNaN : FP
fpQNaN =
    { s = 0, eee = eee111, fff = Dict.insert 0 1 fff000 }


type Msg
    = DoNothing
    | Resize Window.Size
    | SetEdgeEx8 Int
    | SetFP FP
    | FlipS
    | FlipE Int
    | FlipF Int
    | RoundFromNumer Int
    | RoundFromDenom Int
    | RoundFromRoundMode ()


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize


update : Msg -> Model -> Model
update msg model =
    let
        { fp, roundFrom } =
            model

        flipBit b =
            if b == 0 then
                1
            else
                0
    in
        case msg of
            DoNothing ->
                model

            Resize sz ->
                { model | wsize = sz }

            SetEdgeEx8 edgeEx8 ->
                { model | edgeEx8 = edgeEx8 }

            SetFP fp ->
                let
                    ( _, e, _, _ ) =
                        decodeFP fp
                in
                    { model | fp = fp, edgeEx8 = edgeEx8forE e, roundFrom = initRoundFrom }

            FlipS ->
                { model | fp = { fp | s = flipBit fp.s }, roundFrom = initRoundFrom }

            FlipE i ->
                { model | fp = { fp | eee = Dict.update i (Maybe.map flipBit) fp.eee }, roundFrom = initRoundFrom }

            FlipF i ->
                { model | fp = { fp | fff = Dict.update i (Maybe.map flipBit) fp.fff }, roundFrom = initRoundFrom }

            RoundFromNumer n ->
                let
                    roundFromNew =
                        { roundFrom | numer = Just n, roundMode = Nothing }
                in
                    { model | roundFrom = roundFromNew }

            RoundFromDenom n ->
                let
                    roundFromNew =
                        { roundFrom | denom = Just n, roundMode = Nothing }
                in
                    { model | roundFrom = roundFromNew }

            RoundFromRoundMode _ ->
                let
                    roundFromNew =
                        { roundFrom | roundMode = Just () }
                in
                    case ( roundFrom.numer, roundFrom.denom ) of
                        ( Just numer, Just denom ) ->
                            let
                                x =
                                    (toFloat numer) / (toFloat denom)

                                e =
                                    floor <| logBase 2 (abs x)
                            in
                                { model
                                    | roundFrom = roundFromNew
                                    , fp = rat2FP numer denom
                                    , edgeEx8 = min edgeEx8max (max edgeEx8min (edgeEx8forE e))
                                }

                        _ ->
                            model


view : Model -> Html Msg
view model =
    let
        fp =
            model.fp

        ( s, e, lead, m ) =
            decodeFP fp

        fpValue =
            evalFP fp

        fpFinite =
            e + eOffset < 2 ^ eSize - 1

        fpNaN =
            (not fpFinite) && m > 1

        bitFrame msg b =
            H.span [ borderA, fontsizeA 300, E.onClick msg ]
                [ H.text (toString b) ]

        sBCell =
            H.td [ borderA, widthA "10%" ] [ bitFrame FlipS fp.s ]

        eeeBCell =
            H.td [ borderA, widthA "40%" ] (List.map (\( i, b ) -> bitFrame (FlipE i) b) (List.reverse <| Dict.toList fp.eee))

        fffBCell =
            H.td [ borderA, widthA "40%" ] (List.map (\( i, b ) -> bitFrame (FlipF i) b) (List.reverse <| Dict.toList fp.fff))

        bitRow =
            H.tr [] [ sBCell, eeeBCell, fffBCell ]

        sECell =
            H.td [ borderA ]
                [ H.text ("s=" ++ (toString (signB2S fp.s))) ]

        eECell =
            let
                s =
                    if lead == 0 then
                        "e=" ++ (toString e) ++ " (denormalised)"
                    else if fpFinite then
                        "e=" ++ (toString (e + eOffset)) ++ "-" ++ (toString eOffset) ++ "=" ++ (toString e)
                    else
                        "Inifity or NaN"
            in
                H.td [ borderA ] [ H.text s ]

        mECell =
            let
                ( s1, s2, s3 ) =
                    if fpFinite then
                        ( "m=", (toString lead), "." ++ (String.concat <| List.map toString <| List.reverse <| Dict.values fp.fff) ++ "b" )
                    else
                        ( "", "", "" )
            in
                H.td [ borderA ] [ H.text s1, H.span [ A.style [ ( "color", "red" ), ( "font-size", "large" ) ] ] [ H.text s2 ], H.text s3 ]

        emECell =
            let
                shiftBy e l r =
                    if e < 0 then
                        case l of
                            [] ->
                                shiftBy (e + 1) [] (0 :: r)

                            lh :: lt ->
                                shiftBy (e + 1) lt (lh :: r)
                    else if e > 0 then
                        case r of
                            [] ->
                                shiftBy (e - 1) (0 :: l) []

                            rh :: rt ->
                                shiftBy (e - 1) (rh :: l) rt
                    else
                        ( l, r )

                ( lPre, rPre ) =
                    shiftBy e [ lead ] (List.reverse <| Dict.values fp.fff)

                l =
                    case lPre of
                        [] ->
                            [ 0 ]

                        _ ->
                            lPre

                r =
                    case rPre of
                        [] ->
                            [ 0 ]

                        _ ->
                            rPre
            in
                H.td [ borderA, A.colspan 2, A.style [ ( "text-align", "center" ) ] ] <|
                    if fpFinite then
                        [ H.text "2"
                        , H.sup [] [ H.text "e" ]
                        , H.text (" * m = " ++ (String.concat <| List.map toString (List.reverse l) ++ [ "." ] ++ List.map toString r) ++ "b")
                        ]
                    else if fpNaN then
                        [ H.text "NaN" ]
                    else
                        [ H.text "Infinity" ]

        semRow =
            H.tr [] [ sECell, eECell, mECell ]

        shiftRow =
            H.tr [] [ H.td [] [], emECell ]

        resultRow =
            H.tr []
                [ H.td [ borderA, A.colspan 3, A.style [ ( "text-align", "center" ) ] ] <|
                    if fpFinite then
                        [ H.text "s * 2"
                        , H.sup [] [ H.text "e" ]
                        , H.text " * m = "
                        , H.text <| toString <| fpValue
                        ]
                    else if fpNaN then
                        [ H.text "NaN" ]
                    else
                        [ H.text <| (signB2S fp.s) ++ "Infinity" ]
                ]

        fptable =
            H.table [ borderA, widthA "100%" ]
                [ H.tbody [] [ bitRow, semRow, shiftRow, resultRow ] ]

        valButton ( label, fp ) =
            H.td [] [ H.button [ E.onClick (SetFP fp) ] [ H.text label ] ]

        valButtons =
            H.div [] <|
                (++)
                [ H.text "Click on bits to flip them"
                , H.br [] []
                , H.text "or select a special value:"
                ] <|
                    List.map valButton <|
                        [ ( "0", fpZero )
                        , ( "max denormalised", fpMaxDenorm )
                        , ( "min normalised", fpMinNorm )
                        , ( "max", fpMax )
                        , ( "infinity", fpInfinity )
                        , ( "NaN", fpQNaN )
                        ]

        roundFromMaybeX =
            case ( model.roundFrom.numer, model.roundFrom.denom, model.roundFrom.roundMode ) of
                ( Just numer, Just denom, Just () ) ->
                    Just <| (toFloat numer) / (toFloat denom)

                _ ->
                    Nothing

        roundFromEntry =
            let
                formatMaybeN maybeN =
                    case maybeN of
                        Nothing ->
                            ""

                        Just n ->
                            toString n

                numerS =
                    formatMaybeN model.roundFrom.numer

                denomS =
                    formatMaybeN model.roundFrom.denom

                setNumer numerS =
                    RoundFromNumer <| Result.withDefault 1 (String.toInt numerS)

                setDenom denomS =
                    RoundFromDenom <| Result.withDefault 1 (String.toInt denomS)

                roundingReport =
                    case roundFromMaybeX of
                        Just x ->
                            if not fpFinite then
                                "overflow"
                            else if fpValue == 0 && x /= 0 then
                                "underflow"
                            else if fpValue == x then
                                "exact value, ie no rounding"
                            else
                                "rounding by " ++ (toString (fpValue - x))

                        _ ->
                            "..."

                textInput value tagger =
                    H.input
                        [ A.type_ "text"
                        , A.value value
                        , A.size 2
                        , E.on "focusout" (Json.map tagger E.targetValue)
                        ]
                        []
            in
                H.div []
                    [ H.text "or convert the fraction "
                    , textInput numerS setNumer
                    , H.text "/"
                    , textInput denomS setDenom
                    , H.text " rounding "
                    , H.button [ E.onClick (RoundFromRoundMode ()) ] [ H.text "towards 0" ]
                    , H.text <| " leading to " ++ roundingReport
                    ]

        --, ("max", fpMax)]
        ( w, h ) =
            ( model.wsize.width, model.wsize.height // 5 )

        ( wF, hF ) =
            ( toFloat w, (toFloat h) )

        bgr =
            C.filled Color.lightBrown (C.rect wF hF)

        xline =
            let
                maxXC =
                    (evalFP fpMax) * wF / (2 * edgeVal)
            in
                C.traced (C.solid Color.black) (C.segment ( -maxXC, 0 ) ( maxXC, 0 ))

        edgeE =
            toFloat (2 ^ eSize - eOffset - 2)

        edgeVal =
            2 * (2 ^ (toFloat model.edgeEx8 / 8))

        tick level x =
            let
                xc =
                    x * wF / (2 * edgeVal)

                y =
                    hF / (level * 10)
            in
                C.traced (C.solid Color.black) (C.segment ( xc, y ) ( xc, -y ))

        label x n mm =
            let
                xc =
                    x * wF / (2 * edgeVal)

                yn =
                    -2 * hF / 10

                ym =
                    -4 * hF / 10

                theight =
                    hF / 10

                numAt y i =
                    C.move ( xc, y ) <| C.text <| Text.height theight <| Text.fromString (toString i)
            in
                case mm of
                    Nothing ->
                        numAt yn n

                    Just m ->
                        C.group
                            [ numAt yn n
                            , C.traced (C.solid Color.black) <| C.segment ( xc - theight, (ym + yn) / 2 ) ( xc + theight, (ym + yn) / 2 )
                            , numAt ym m
                            ]

        pointer x =
            let
                xc =
                    x * wF / (2 * edgeVal)
            in
                C.outlined (C.solid Color.red)
                    (C.polygon [ ( xc, 2 * hF / 10 ), ( xc - wF / 50, 4 * hF / 10 ), ( xc + wF / 50, 4 * hF / 10 ) ])

        ( exponents, fractions ) =
            let
                addBits i ees =
                    if i < 0 then
                        ees
                    else
                        addBits (i - 1) <| (List.map (Dict.insert i 0) ees) ++ (List.map (Dict.insert i 1) ees)
            in
                ( addBits (eSize - 1) [ Dict.empty ]
                , addBits (fSize - 1) [ Dict.empty ]
                )

        edgePoints =
            List.map evalFP <| List.map (\eee -> { fpZero | s = fp.s, eee = eee }) exponents

        dropLast list =
            List.take (List.length list - 1) list

        allPoints =
            List.map evalFP <|
                List.concat <|
                    (flip List.map) (dropLast exponents) <|
                        \eee ->
                            (flip List.map) fractions <|
                                \fff ->
                                    { fpZero | s = fp.s, eee = eee, fff = fff }

        edgePointLabel ( x, eee ) =
            let
                s =
                    if x < 0 then
                        -1
                    else
                        1

                e =
                    bits2Int eee - eOffset
            in
                if x == 0 then
                    label x 0 Nothing
                else if e + eOffset == 2 ^ eSize - 1 then
                    label x s (Just 0)
                else if e >= 0 then
                    label x (s * 2 ^ e) Nothing
                else
                    label x s (Just <| 2 ^ (-e))

        numberline =
            collage w h <|
                [ bgr, xline, pointer fpValue ]
                    ++ List.map (tick 2) allPoints
                    ++ List.map (tick 1) edgePoints
                    ++ (List.map edgePointLabel <| List.map2 (\a b -> ( a, b )) edgePoints exponents)
    in
        H.div []
            [ valButtons
            , roundFromEntry
            , fptable
            , Element.toHtml <| numberline
            , H.text "zoom:"
            , slider
                { makeMsg = SetEdgeEx8
                , minValue = edgeEx8min
                , maxValue = edgeEx8max
                , model = model.edgeEx8
                }
            , H.br [] []
            , H.text "(c) 2018 Michal Konečný, Aston University, "
            , H.a [ A.href "http://elm-lang.org/", A.target "_blank" ] [ H.text "powered by Elm" ]
            ]


borderA : H.Attribute msg
borderA =
    A.style [ ( "border", "1px solid black" ) ]


widthA : String -> H.Attribute msg
widthA w =
    A.style [ ( "width", w ) ]


fontsizeA : Int -> H.Attribute msg
fontsizeA percent =
    A.style [ ( "font-size", (toString percent) ++ "%" ) ]


slider : { makeMsg : Int -> Msg, minValue : Int, maxValue : Int, model : Int } -> Html Msg
slider { makeMsg, minValue, maxValue, model } =
    H.input
        [ A.type_ "range"
        , A.min <| toString minValue
        , A.max <| toString maxValue
        , A.value <| toString model
        , widthA "80%"
        , E.onInput (makeMsg << Result.withDefault minValue << String.toInt)
        ]
        []
