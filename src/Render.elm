module
    Render
    ( render
    )
    where

import Node exposing (..)
import SUS exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (Content, defaultStyle, field)
import Signal exposing (constant)
import Text exposing (fromString)
import Window


render : Signal Node -> Signal Element
render nodes =
    Signal.map2 renderRoot
        nodes
        (Signal.map (tupleMap toFloat) Window.dimensions)



-- INTERNAL


collage' w h fs =
    collage (ceiling w) (ceiling h) fs


move' ( w, h ) form =
    move ( -w, h ) form


renderRoot : Node -> Sizes -> Element
renderRoot node ( sceneW, sceneH ) =
    let
        sceneSize =
            ( sceneW, sceneH )
    in
        case node.status of
            Enabled ->
                case node.nodeType of
                    Rect def ->
                        let
                            rend : Rend'
                            rend =
                                rendRect def
                                    sceneSize
                                    (tupleMap (Just) sceneSize)
                                    True

                            rect : Form
                            rect =
                                toForm rend.element

                            relatives : List Form
                            relatives =
                                rend.relatives

                            popups : List Form
                            popups =
                                rend.popups

                            parts : List Form
                            parts =
                                rect :: popups ++ relatives
                        in
                            collage' sceneW sceneH parts

                    Text def ->
                        renderText def

                    InputText def ->
                        renderInputText def

                    SUS def ->
                        renderRoot (susDefToNode def) sceneSize

            Disabled ->
                empty



{-
   Rendering order is from bottom up, so that the root node is the last thing to
   get rendered.

                     ┌──────────────────────────┐
                     V                          │
   renderRoot -> rendRect -> rendChildren -> rendChild
-}


tupleMap : (a -> b) -> ( a, a ) -> ( b, b )
tupleMap f ( x, y ) =
    ( f x, f y )


type alias MaybeSizes =
    ( Maybe Size, Maybe Size )


type alias Rend' =
    RendInto' Element


type alias RendInto' format =
    { element : format
    , relatives : List Form
    , popups : List Form
    }


rendRect : RectDef -> Sizes -> MaybeSizes -> Bool -> Rend'
rendRect def sceneSize parentSize isRoot =
    let
        ( tb, rb, bb, lb ) =
            borderSize def.border

        bs =
            ( tb, rb, bb, lb )

        maybeSize : MaybeSizes
        maybeSize =
            tupleMap2 tryGetSize def.extents parentSize

        ( renderChildrenFn, moveChildrenFn ) =
            case def.dir of
                Up s ->
                    ( rendChildren s Vert, moveChildren s Vert True )

                Down s ->
                    ( rendChildren s Vert, moveChildren s Vert False )

                Left s ->
                    ( rendChildren s Hori, moveChildren s Hori True )

                Right s ->
                    ( rendChildren s Hori, moveChildren s Hori False )

                In ->
                    ( rendStackChildren, moveStackChildren True )

                Out ->
                    ( rendStackChildren, moveStackChildren False )

        -- (children, childrenSize) : (List Rend', MaybeSizes)
        ( children, childrenSize ) =
            renderChildrenFn sceneSize
                -- children size reduced by border size
                (tupleMapEach (Maybe.map (\w -> w - rb - lb))
                    (Maybe.map (\h -> h - tb - bb))
                    maybeSize
                )
                bs
                def.children

        size : Sizes
        size =
            tupleMap2 getSize maybeSize childrenSize

        ( width, height ) =
            ( fst size, snd size )

        border : List Form
        border =
            case def.border of
                Just bs ->
                    renderRectBorder bs size

                _ ->
                    []

        bgs : List Form
        bgs =
            List.map (renderRectBg width height) <| List.reverse def.bgs

        movedCs : List (RendInto' Form)
        movedCs =
            moveChildrenFn size ( tb, rb, bb, lb ) children

        rend : List Form -> Element
        rend cs =
            collage' width height cs

        cPopups : List Form
        cPopups =
            List.concatMap (\c -> c.popups) movedCs

        cRelatives : List Form
        cRelatives =
            List.concatMap (\c -> c.relatives) movedCs

        rendAsRoot : Node -> Rend'
        rendAsRoot r =
            rendChild sceneSize (tupleMap Just sceneSize) r

        rendPopups : List Rend'
        rendPopups =
            List.map rendAsRoot def.popups

        popups : List Form
        popups =
            List.map (\p -> toForm p.element) rendPopups

        pPopups : List Form
        pPopups =
            List.concatMap (\p -> p.popups) rendPopups

        pRelatives : List Form
        pRelatives =
            List.concatMap (\p -> p.relatives) rendPopups

        alignRelative : Element -> Sizes -> Form
        alignRelative e ( offW, offH ) =
            let
                ( w, h ) =
                    tupleMap toFloat (sizeOf e)
            in
                move' ( -w * 0.5 - offW, -h * 0.5 - offH ) (toForm e)

        rendRelatives : List ( Rend', Sizes )
        rendRelatives =
            List.map
                (\( node, offset ) ->
                    ( rendAsRoot node, offset )
                )
                def.relatives

        relatives : List Form
        relatives =
            List.map
                (\( rend, offset ) ->
                    alignRelative rend.element offset
                )
                rendRelatives

        rPopups : List Form
        rPopups =
            List.concatMap (\( r, _ ) -> r.popups) rendRelatives

        rRelatives : List Form
        rRelatives =
            List.concatMap (\( r, _ ) -> r.relatives) rendRelatives

        cs : List Form
        cs =
            List.map (\c -> c.element) movedCs

        rendered : Element
        rendered =
            rend (bgs ++ border ++ cs)

        moveRelatives : List Form -> List Form
        moveRelatives =
            if isRoot then
                List.map (move' ( width * 0.5, height * 0.5 ))
            else
                identity
    in
        { element = rendered
        , relatives =
            moveRelatives
                (pRelatives ++ rRelatives ++ cRelatives ++ relatives)
        , popups = pPopups ++ rPopups ++ cPopups ++ popups
        }


tupleMapEach : (a -> b) -> (c -> d) -> ( a, c ) -> ( b, d )
tupleMapEach f g ( x, y ) =
    ( f x, g y )


renderRectBorder : BorderStyle -> Sizes -> List Form
renderRectBorder bs ( width, height ) =
    let
        rendSame size =
            [ Graphics.Collage.outlined
                { defaultLine | color = bs.color, width = size * 2.0 }
                (renderRect width height)
            ]

        rendBorder size ( sW, sH ) ( eW, eH ) =
            Graphics.Collage.traced
                { defaultLine | color = bs.color, width = size * 2.0 }
                (Graphics.Collage.segment
                    ( sW * width * 0.5, sH * height * 0.5 )
                    ( eW * width * 0.5, eH * height * 0.5 )
                )

        rendDiff t r b l =
            [ rendBorder r ( 1, 1 ) ( 1, -1 )
            , rendBorder l ( -1, -1 ) ( -1, 1 )
            , rendBorder t ( -1, 1 ) ( 1, 1 )
            , rendBorder b ( 1, -1 ) ( -1, -1 )
            ]
    in
        case bs.thickness of
            All a ->
                rendSame a

            HoriVert h v ->
                if h == v then
                    rendSame h
                else
                    rendDiff h v h v

            TRBL t r b l ->
                if t == r && t == b && t == l then
                    rendSame t
                else
                    rendDiff t r b l


renderRectBg : Size -> Size -> Background -> Form
renderRectBg width height bg =
    case bg of
        Filled c ->
            Graphics.Collage.filled c (renderRect width height)

        Textured s ->
            Graphics.Collage.textured s (renderRect width height)

        Gradient g ->
            Graphics.Collage.gradient g (renderRect width height)


renderRect : Size -> Size -> Shape
renderRect width height =
    Graphics.Collage.rect width height


tupleMap2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
tupleMap2 f ( x, y ) ( w, z ) =
    ( f x w, f y z )


tryGetSize : Extent -> Maybe Size -> Maybe Size
tryGetSize extent parentSize =
    case extent of
        Fix s ->
            Just s

        Fit ->
            Nothing

        Fill ratio ->
            Maybe.map ((*) ratio) parentSize


getSize : Maybe Size -> Maybe Size -> Size
getSize maybeSize childrenSize =
    Maybe.withDefault (Maybe.withDefault 0.0 childrenSize) maybeSize


rendChildren :
    Spacing
    -> Side
    -> Sizes
    -> MaybeSizes
    -> ( Size, Size, Size, Size )
    -> List Node
    -> ( List Rend', MaybeSizes )
rendChildren spacing side sceneSize parentSize ( tb, rb, bb, lb ) nodes =
    let
        ( fills, fixesAndFits ) =
            List.partition ((onWhich side) |> extentIsFill) nodes

        fillCount : Int
        fillCount =
            fills |> List.length

        adjOtherParentSize : Maybe Size
        adjOtherParentSize =
            recalcParentSize side
                sceneSize
                parentSize
                fixesAndFits
                fillCount

        adjParentSize : MaybeSizes
        adjParentSize =
            case side of
                Hori ->
                    ( adjOtherParentSize, snd parentSize )

                Vert ->
                    ( fst parentSize, adjOtherParentSize )

        children : List Rend'
        children =
            List.map (rendChild sceneSize adjParentSize) nodes

        childrenSizes : List Sizes
        childrenSizes =
            List.map
                (\c ->
                    let
                        ( w, h ) =
                            tupleMap toFloat (sizeOf c.element)
                    in
                        ( w + rb + lb, h + tb + bb )
                )
                children

        spacingCount : Float
        spacingCount =
            let
                childrenCount =
                    List.length children
            in
                toFloat
                    (if childrenCount > 0 then
                        childrenCount - 1
                     else
                        0
                    )

        spacings : Spacing
        spacings =
            spacing * spacingCount

        ( compareW, compareH ) =
            case side of
                Hori ->
                    ( (+), max )

                Vert ->
                    ( max, (+) )

        ( sizeW, sizeH ) =
            foldl2ToTuples compareW compareH childrenSizes

        sizes : MaybeSizes
        sizes =
            case side of
                Hori ->
                    ( Maybe.map ((+) spacings) sizeW, sizeH )

                Vert ->
                    ( sizeW, Maybe.map ((+) spacings) sizeH )
    in
        ( children, sizes )


type Side
    = Vert
    | Hori


onWhich : Side -> Which a
onWhich side =
    case side of
        Hori ->
            fst

        Vert ->
            snd


rendStackChildren :
    Sizes
    -> MaybeSizes
    -> ( Size, Size, Size, Size )
    -> List Node
    -> ( List Rend', MaybeSizes )
rendStackChildren sceneSize parentSize ( tb, rb, bb, lb ) nodes =
    let
        children : List Rend'
        children =
            List.map (rendChild sceneSize parentSize) nodes

        childrenSizes : List Sizes
        childrenSizes =
            List.map
                (\c ->
                    let
                        ( w, h ) =
                            tupleMap toFloat (sizeOf c.element)
                    in
                        ( w + rb + lb, h + tb + bb )
                )
                children
    in
        ( children, foldl2ToTuples max max childrenSizes )


foldl2ToTuples :
    (Float -> Float -> Float)
    -> (Float -> Float -> Float)
    -> List Sizes
    -> MaybeSizes
foldl2ToTuples fn1 fn2 a =
    case a of
        x :: xs ->
            let
                w : Size
                w =
                    List.foldl fn1 (fst x) (List.map fst xs)

                h : Size
                h =
                    List.foldl fn2 (snd x) (List.map snd xs)
            in
                ( Just w, Just h )

        _ ->
            ( Nothing, Nothing )


recalcParentSize :
    Side
    -> Sizes
    -> MaybeSizes
    -> List Node
    -> Int
    -> Maybe Size
recalcParentSize side sceneSize parentSize nodes fillCount =
    if fillCount == 0 then
        (onWhich side) parentSize
    else
        let
            ( fixes, fits ) =
                List.partition ((onWhich side) |> extentIsFix) nodes

            fixesSize =
                List.map ((onWhich side) |> fixSize) fixes |> List.sum

            recalc size =
                (size - fixesSize) / (toFloat fillCount)
        in
            Maybe.map
                (\s ->
                    if fillCount > 0 then
                        recalc s
                    else
                        s
                )
                ((onWhich side) parentSize)


rendChild : Sizes -> MaybeSizes -> Node -> Rend'
rendChild sceneSize parentSize node =
    case node.status of
        Enabled ->
            case node.nodeType of
                Rect def ->
                    rendRect def sceneSize parentSize False

                Text def ->
                    renderText def |> elemToRend'

                InputText def ->
                    renderInputText def |> elemToRend'

                SUS def ->
                    rendChild sceneSize parentSize (susDefToNode def)

        Disabled ->
            elemToRend' empty


elemToRend' : Element -> Rend'
elemToRend' e =
    { element = e
    , relatives = []
    , popups = []
    }


moveChildren :
    Spacing
    -> Side
    -> Bool
    -> Sizes
    -> ( Size, Size, Size, Size )
    -> List Rend'
    -> List (RendInto' Form)
moveChildren spacing side reverse ( parentW, parentH ) ( tb, rb, bb, lb ) children =
    let
        moveChild : Rend' -> Size -> ( RendInto' Form, Size )
        moveChild child offset =
            let
                c : Element
                c =
                    child.element

                childW =
                    widthOf c |> toFloat

                childH =
                    heightOf c |> toFloat

                ( ( w, h ), nextOffset ) =
                    case side of
                        Vert ->
                            ( ( -lb, offset - tb ), offset - childH - spacing )

                        Hori ->
                            ( ( offset - lb, -tb ), offset - childW - spacing )

                -- align at top left corner
                childMov =
                    ( w + (parentW - childW) * 0.5
                    , h + (parentH - childH) * 0.5
                    )
            in
                ( { child
                    | element = move' childMov (toForm c)
                    , relatives = List.map (move' ( w, h )) child.relatives
                  }
                , nextOffset
                )

        fold =
            case reverse of
                True ->
                    List.foldr

                False ->
                    List.foldl
    in
        fold
            (\x acc ->
                (moveChild x
                    -- get the offset from the previous child if any
                    (List.head acc
                        |> Maybe.map snd
                        |> Maybe.withDefault 0.0
                    )
                )
                    :: acc
            )
            []
            children
            -- throw away the offsets
            |>
                List.map fst


moveStackChildren :
    Bool
    -> Sizes
    -> ( Size, Size, Size, Size )
    -> List Rend'
    -> List (RendInto' Form)
moveStackChildren reverse ( parentW, parentH ) ( tb, rb, bb, lb ) children =
    let
        moveChild : Rend' -> RendInto' Form
        moveChild child =
            let
                c : Element
                c =
                    child.element

                childW =
                    widthOf c |> toFloat

                childH =
                    heightOf c |> toFloat

                childMov : Sizes
                childMov =
                    ( -lb + (parentW - childW) * 0.5
                    , -tb + (parentH - childH) * 0.5
                    )
            in
                { child
                    | element = move' childMov (toForm c)
                    , relatives = List.map (move' ( -lb, -tb )) child.relatives
                }

        cs' =
            case reverse of
                True ->
                    List.reverse children

                False ->
                    children
    in
        List.map moveChild cs'


renderText : TextDef -> Element
renderText def =
    leftAligned def.text


renderInputText : InputTextDef -> Element
renderInputText def =
    field defaultStyle def.handler def.name def.content



{-
   renderInputText : InputTextDef -> Signal Element
   renderInputText def = Signal.map (\c ->
       Html.toElement 100 50 (Html.input
           [ placeholder def.name
           ,value c
           ,on "input" targetValue def.handler
          ] [])) def.content
-}
