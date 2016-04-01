module Render
    ( render
    ) where

import Node                 exposing ( .. )
import SUS                  exposing ( .. )

import Graphics.Collage     exposing ( .. )
import Graphics.Element     exposing ( .. )
import Graphics.Input.Field exposing ( Content, defaultStyle, field )
import Window
import Text                 exposing ( fromString )

render : Node -> Signal Element
render node = 
    renderRoot node ( Signal.map ( tupleMap toFloat ) Window.dimensions )

-- INTERNAL

collage' w h fs = collage ( ceiling w ) ( ceiling h ) fs
move' ( w, h ) form = move ( -w, h ) form

renderRoot : Node -> Signal Sizes -> Signal Element
renderRoot node sceneSizes =
    case node.nodeType of
        Rect def -> 
            let rend : Rend
                rend = rendRect def sceneSizes
                    ( Signal.map ( tupleMap ( Just ) ) sceneSizes ) True
                rect : Signal Form
                rect = Signal.map toForm rend.element
                relatives : List ( Signal Form )
                relatives = rend.relatives
                popups : List ( Signal Form )
                popups = rend.popups
                parts : Signal ( List Form )
                parts = combine ( rect :: popups ++ relatives )
            in Signal.map2 ( \( sceneW, sceneH ) part -> 
                    collage' sceneW sceneH part ) sceneSizes parts
        Text def -> Signal.constant ( renderText def )
        InputText def -> renderInputText def
        SUS def -> renderRoot ( susDefToNode def ) sceneSizes

combine : List ( Signal a ) -> Signal ( List a )
combine = List.foldr ( Signal.map2 ( :: ) ) ( Signal.constant [ ] )

{-
Rendering order is from bottom up, so that the root node is the last thing to 
get rendered.

renderRoot : Element
-> rendRect : Rend <-------------|
-> rendChildren : List Rend      |
-> rendChild : Rend -------------|
-}

tupleMap : ( a -> b ) -> ( a, a ) -> ( b, b )
tupleMap f ( x, y ) = ( f x, f y )

type alias MaybeSizes = ( Maybe Size, Maybe Size )

type alias Rend = RendInto Element
type alias RendInto format =
    { element : Signal format
    , relatives : List ( Signal Form )
    , popups : List ( Signal Form )
    } 

rendRect : RectDef -> Signal Sizes -> Signal MaybeSizes -> Bool -> Rend
rendRect def sceneSizes parentSizes isRoot =
    let ( tb, rb, bb, lb ) = borderSize def.border
        maybeSizes : Signal MaybeSizes
        maybeSizes = Signal.map ( tupleMap2 tryGetSize def.extents ) parentSizes
        ( renderChildrenFn, moveChildrenFn ) = case def.dir of
            Up s -> ( rendChildren s Vert, moveChildren s Vert True )
            Down s -> ( rendChildren s Vert, moveChildren s Vert False )
            Left s -> ( rendChildren s Hori, moveChildren s Hori True )
            Right s -> ( rendChildren s Hori, moveChildren s Hori False )
            In -> ( rendStackChildren, moveStackChildren True )
            Out -> ( rendStackChildren, moveStackChildren False )
        -- ( children, childrenSizes ) : ( List Rend, Signal MaybeSizes )
        ( children, childrenSizes ) = 
            renderChildrenFn sceneSizes
                -- children size reduced by border size
                ( Signal.map ( tupleMapEach 
                        ( Maybe.map ( \w -> w - rb - lb ) )
                        ( Maybe.map ( \h -> h - tb - bb ) ) ) maybeSizes )
                def.children
        sizes : Signal Sizes
        --( width, height ) = Signal.map2 ( \( maybeSize, childrenSize ) -> 
        sizes = Signal.map2 ( \maybeSize childrenSize -> 
                    tupleMap2 getSize maybeSize childrenSize ) 
                maybeSizes childrenSizes
        border : List ( Signal Form )
        border = case def.border of
            Just bs -> renderRectBorder bs sizes
            _ -> []
        bgs : List ( Signal Form )
        bgs = List.map ( \bgs' -> 
                Signal.map2 ( \( width, height ) bg -> 
                    renderRectBg bg width height ) sizes bgs' ) def.bgs
        movedCs : List ( RendInto Form )
        movedCs = moveChildrenFn sizes ( tb, rb, bb, lb ) children
        rend : List ( Signal Form ) -> Signal Element
        rend cs' = Signal.map2 ( \( width, height ) c -> 
                collage' width height c ) sizes
            -- List ( Signal Form ) => Signal ( List Form )
            ( List.foldr ( \next acc -> Signal.map2 ( :: ) next acc ) 
                ( Signal.constant [ ] ) cs' )
        cPopups : List ( Signal Form )
        cPopups = List.concatMap ( \c -> c.popups ) movedCs
        cRelatives : List ( Signal Form )
        cRelatives = List.concatMap ( \c -> c.relatives ) movedCs
        rendAsRoot : Node -> Rend
        rendAsRoot r = rendChild sceneSizes 
                ( Signal.map ( tupleMap Just ) sceneSizes ) r
        rendPopups : List Rend
        rendPopups = List.map rendAsRoot def.popups
        popups : List ( Signal Form )
        popups = List.map ( \p -> Signal.map toForm p.element ) rendPopups
        pPopups : List ( Signal Form )
        pPopups = List.concatMap ( \p -> p.popups ) rendPopups
        pRelatives : List ( Signal Form )
        pRelatives = List.concatMap ( \p -> p.relatives ) rendPopups
        alignRelative : Rend -> Sizes -> Signal Form
        alignRelative rend ( offW, offH ) = 
            Signal.map ( \e ->
                let ( w, h ) = tupleMap toFloat ( sizeOf e )
                in  move' ( -w * 0.5 - offW, -h * 0.5 - offH ) ( toForm e ) ) 
            rend.element
        rendRelatives : List ( Rend, Sizes )
        rendRelatives = List.map ( \( node, offset ) -> 
                ( rendAsRoot node, offset ) ) def.relatives
        relatives : List ( Signal Form )
        relatives = List.map ( \( rend, offset ) -> 
                alignRelative rend offset )  rendRelatives
        rPopups : List ( Signal Form )
        rPopups = List.concatMap ( \( r, _ ) -> r.popups ) rendRelatives
        rRelatives : List ( Signal Form )
        rRelatives = List.concatMap ( \( r, _ ) -> r.relatives ) rendRelatives
        cs : List ( Signal Form )
        cs = List.map ( \c -> c.element ) movedCs
        rendered : Signal Element
        rendered = rend ( bgs ++ border ++ cs )
        moveRelatives : List ( Signal Form ) -> List ( Signal Form )
        moveRelatives = if isRoot 
            then List.map ( Signal.map2 ( \( width, height ) r -> 
                    ( move' ( width * 0.5, height * 0.5 ) r ) ) sizes )
            else identity 
    in  { element = rendered
        , relatives = moveRelatives 
            ( pRelatives ++ rRelatives ++ cRelatives ++ relatives )
        , popups = pPopups ++ rPopups ++ cPopups ++ popups
        }

tupleMapEach : ( a -> b ) -> ( c -> d ) -> ( a, c ) -> ( b, d )
tupleMapEach f g ( x, y ) = ( f x, g y )

renderRectBorder : BorderStyle -> Signal Sizes -> List ( Signal Form )
renderRectBorder bs sizes = 
    let rendSame size = [ Signal.map ( \( w, h ) -> 
            ( Graphics.Collage.outlined 
                { defaultLine | color = bs.color, width = size * 2.0 } 
                ( renderRect w h ) ) ) sizes ]
        rendBorder size ( sW, sH ) ( eW, eH ) = Signal.map ( \( w, h ) -> 
            Graphics.Collage.traced
                { defaultLine | color = bs.color, width = size * 2.0 }
                ( Graphics.Collage.segment 
                    ( sW * w * 0.5, sH * h * 0.5 ) 
                    ( eW * w * 0.5, eH * h * 0.5 ) ) ) sizes
        rendDiff t r b l = 
            [ rendBorder r (  1,  1 ) (  1, -1 )
            , rendBorder l ( -1, -1 ) ( -1,  1 )
            , rendBorder t ( -1,  1 ) (  1,  1 )
            , rendBorder b (  1, -1 ) ( -1, -1 ) ]
    in case bs.thickness of
        All a -> rendSame a 
        HoriVert h v -> if h == v then rendSame h else rendDiff h v h v
        TRBL t r b l -> if t == r && t == b && t == l
                then rendSame t 
                else rendDiff t r b l

renderRectBg : Background -> Size -> Size -> Form
renderRectBg bg width height = case bg of
        Filled c -> Graphics.Collage.filled c ( renderRect width height )
        Textured s -> Graphics.Collage.textured s ( renderRect width height )
        Gradient g -> Graphics.Collage.gradient g ( renderRect width height )

renderRect : Size -> Size -> Shape
renderRect width height = 
    Graphics.Collage.rect width height

tupleMap2 : ( a -> b -> c ) -> ( a, a ) -> ( b, b ) -> ( c, c )
tupleMap2 f ( x, y ) ( w, z ) = ( f x w, f y z )

tryGetSize : Extent -> Maybe Size -> Maybe Size
tryGetSize extent parentSize = case extent of
            Fix s -> Just s
            Fit -> Nothing
            Fill ratio -> Maybe.map ( ( * ) ratio ) parentSize

getSize : Maybe Size -> Maybe Size -> Size
getSize maybeSize childrenSize =
    Maybe.withDefault ( Maybe.withDefault 0.0 childrenSize ) maybeSize 

rendChildren : Spacing -> Side -> Signal Sizes -> Signal MaybeSizes -> List Node 
   -> ( List Rend, Signal MaybeSizes )
rendChildren spacing side sceneSizes parentSizes nodes =
    let ( fills, fixesAndFits ) = 
            List.partition ( ( onWhich side ) |> extentIsFill ) nodes
        fillCount : Int
        fillCount = fills |> List.length
        adjOtherParentSizes : Signal ( Maybe Size ) 
        adjOtherParentSizes = Signal.map2 ( \sceneSize parentSize ->
            recalcParentSize side sceneSize parentSize fixesAndFits fillCount )
            sceneSizes parentSizes
        adjParentSizes : Signal ( MaybeSizes )
        adjParentSizes = case side of
            Hori -> Signal.map2 ( \adjOtherParentSize parentSize ->
                    ( adjOtherParentSize, snd parentSize ) ) 
                adjOtherParentSizes parentSizes
            Vert -> Signal.map2 ( \adjOtherParentSize parentSize ->
                    ( fst parentSize, adjOtherParentSize ) ) 
                adjOtherParentSizes parentSizes
        children : List Rend
        children = List.map ( rendChild sceneSizes adjParentSizes ) nodes
        childrenSizes : List ( Signal Sizes )
        childrenSizes = List.map ( \c -> Signal.map ( \e -> 
                tupleMap toFloat ( sizeOf e ) ) c.element ) children
        spacingCount : Float
        spacingCount = 
            let childrenCount = List.length children
            in  toFloat ( if childrenCount > 0 then childrenCount - 1 else 0 )
        spacings : Spacing
        spacings = spacing * spacingCount
        ( compareW, compareH ) = case side of
            Hori -> ( ( + ), max )
            Vert -> ( max, ( + ) )
        sizes : Signal MaybeSizes
        sizes = Signal.map ( \( sizeW, sizeH ) -> 
                case side of
                    Hori -> ( Maybe.map ( ( + ) spacings ) sizeW, sizeH )
                    Vert -> ( sizeW, Maybe.map ( ( + ) spacings ) sizeH ) ) 
            ( liftList compareW compareH childrenSizes )
    in ( children, sizes )

type Side = Vert | Hori

onWhich : Side -> Which a
onWhich side = case side of
        Hori -> fst
        Vert -> snd

rendStackChildren : Signal Sizes -> Signal MaybeSizes -> List Node 
    -> ( List Rend, Signal MaybeSizes )
rendStackChildren sceneSizes parentSizes nodes =
    let children : List Rend
        children = 
            List.map ( rendChild sceneSizes parentSizes ) nodes
        childrenSizes : List ( Signal Sizes )
        childrenSizes = List.map ( \c -> Signal.map ( \e -> 
                tupleMap toFloat ( sizeOf e ) ) c.element ) children
    in ( children, liftList max max childrenSizes )

liftList : ( Float -> Float -> Float ) -> ( Float -> Float -> Float ) 
    -> List ( Signal Sizes ) -> Signal MaybeSizes
liftList fn1 fn2 a = case a of
        x :: xs -> 
                let ws : Signal Size
                    ws = List.foldl ( Signal.map2 fn1 ) ( Signal.map fst x ) 
                        ( List.map ( Signal.map fst ) xs ) 
                    hs : Signal Size
                    hs = List.foldl ( Signal.map2 fn2 ) ( Signal.map snd x ) 
                        ( List.map ( Signal.map snd ) xs )
                in  Signal.map2 ( \w h -> ( Just w, Just h ) ) ws hs
        _ -> Signal.constant ( Nothing, Nothing )

recalcParentSize : Side -> Sizes -> MaybeSizes -> List Node -> Int 
        -> Maybe Size
recalcParentSize side sceneSize parentSize nodes fillCount =
    if fillCount == 0
        then ( onWhich side ) parentSize
        else
            let ( fixes, fits ) = 
                    List.partition ( ( onWhich side ) |> extentIsFix ) nodes
                fixesSize = 
                    List.map ( ( onWhich side ) |> fixSize ) fixes |> List.sum
                recalc size = ( size - fixesSize ) / ( toFloat fillCount )
            in Maybe.map ( \s -> if fillCount > 0 then recalc s else s ) 
                ( ( onWhich side ) parentSize )

rendChild : Signal Sizes -> Signal MaybeSizes -> Node -> Rend
rendChild sceneSizes parentSizes node =
     case node.nodeType of
        Rect def -> rendRect def sceneSizes parentSizes False
        Text def -> renderText def |> Signal.constant |> elemToRend
        InputText def -> renderInputText def |> elemToRend
        SUS def -> rendChild sceneSizes parentSizes ( susDefToNode def )

elemToRend : Signal Element -> Rend
elemToRend e =
    { element = e
    , relatives = []
    , popups = []
    }

moveChildren : Spacing -> Side -> Bool -> Signal Sizes 
         -> ( Size, Size, Size, Size ) -> List Rend -> List ( RendInto Form )
moveChildren spacing side reverse parentSizes ( tb, rb, bb, lb ) 
    children = 
    let moveChild : Rend -> Signal Size -> ( RendInto Form, Signal Size )
        moveChild child offsets = 
            let cs : Signal Element
                cs = child.element
                childSizes : Signal Sizes
                childSizes = Signal.map ( \c ->
                        ( widthOf c |> toFloat, heightOf c |> toFloat ) ) cs
                ( ( ws, hs ), nextOffsets ) = case side of
                    Vert ->
                        ( ( Signal.constant -lb, 
                            Signal.map ( \offset -> offset - tb ) offsets )
                        , Signal.map2 ( \( _, childH ) offset -> 
                                offset - childH - spacing ) childSizes offsets )
                    Hori ->
                        ( ( Signal.map ( \offset -> offset - lb ) offsets, 
                            Signal.constant -tb )
                        , Signal.map2 ( \( childW, _ ) offset -> 
                                offset - childW - spacing ) childSizes offsets )
                -- align at top left corner
                childMovs = Signal.map4 
                    ( \w h ( parentW, parentH ) ( childW, childH ) ->
                        ( w + ( parentW - childW ) * 0.5
                        , h + ( parentH - childH ) * 0.5 ) )
                    ws hs parentSizes childSizes
            in  (   { child 
                    | element = Signal.map2 ( \c childMov -> 
                        move' childMov ( toForm c ) ) cs childMovs
                    , relatives = List.map
                        ( Signal.map3 ( \w h r -> move' ( w, h ) r ) ws hs ) 
                        child.relatives
                    }
                , nextOffsets )
        fold = case reverse of
            True -> List.foldr
            False -> List.foldl
    in  fold ( \x acc -> 
            ( moveChild x 
                -- get the offset from the previous child if any
                ( List.head acc 
                    |> Maybe.map snd 
                    |> Maybe.withDefault ( Signal.constant 0.0 ) ) 
            ) :: acc ) 
            [] children 
                -- throw away the offsets
                |> List.map fst

moveStackChildren : Bool -> Signal Sizes -> ( Size, Size, Size, Size ) 
         -> List Rend -> List ( RendInto Form )
moveStackChildren reverse parentSizes ( tb, rb, bb, lb ) children = 
    let moveChild : Rend -> RendInto Form
        moveChild child = 
            let cs : Signal Element
                cs = child.element
                childSizes : Signal Sizes
                childSizes = Signal.map ( \c ->
                        ( widthOf c |> toFloat, heightOf c |> toFloat ) ) cs
                childMovs : Signal Sizes
                childMovs = Signal.map2 
                    ( \( parentW, parentH ) ( childW, childH ) ->
                        ( -lb + ( parentW - childW ) * 0.5
                        , -tb + ( parentH - childH ) * 0.5 ) )
                    parentSizes childSizes
            in  { child 
                | element = Signal.map2 ( \c childMov -> 
                        move' childMov ( toForm c ) ) cs childMovs
                , relatives = List.map 
                    ( Signal.map ( move' ( -lb, -tb ) ) ) child.relatives 
                }
        cs' = case reverse of
            True -> List.reverse children
            False -> children
    in List.map moveChild cs'

renderText : TextDef -> Element
renderText def = leftAligned def.text

renderInputText : InputTextDef -> Signal Element
renderInputText def = Signal.map ( field defaultStyle def.handler def.name ) 
    def.content
{-
renderInputText : InputTextDef -> Signal Element
renderInputText def = Signal.map ( \c ->
    Html.toElement 100 50 ( Html.input
        [ placeholder def.name
        , value c
        , on "input" targetValue def.handler
        ] [ ] ) ) def.content
-}

