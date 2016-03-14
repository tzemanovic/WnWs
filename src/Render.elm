module Render
    ( render
    ) where

import Node             exposing ( .. )

import Graphics.Element exposing ( .. )
import Graphics.Collage exposing ( .. )
import Window
import Text             exposing ( fromString )

render : Node -> Signal Element
render node = Signal.map ( renderRoot node ) 
        ( Signal.map ( tupleMap toFloat ) Window.dimensions )

-- INTERNAL

collage' w h fs = collage ( ceiling w ) ( ceiling h ) fs
move' ( w, h ) form = move ( -w, h ) form

renderRoot : Node -> Sizes -> Element
renderRoot node ( sceneW, sceneH ) =
    let sceneSize = ( sceneW, sceneH )
    in case node.nodeType of
            Rect def -> 
                let rend = rendRect def sceneSize
                        ( tupleMap ( Just ) sceneSize ) True
                    rect = rend.element
                    relatives = rend.relatives
                    popups = rend.popups
                in collage' sceneW sceneH ( toForm ( rect ) :: popups ++ relatives )
            Text def -> renderText def

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
    { element : format
    , relatives : List Form
    , popups : List Form
    } 

rendRect : RectDef -> Sizes -> MaybeSizes -> Bool -> Rend
rendRect def sceneSize parentSize isRoot =
    let ( tb, rb, bb, lb ) = borderSize def.border
        maybeSize = tupleMap2 tryGetSize def.extents parentSize
        ( renderChildrenFn, moveChildrenFn ) = case def.dir of
            Up s -> ( rendChildren Vert, moveChildren s Vert True )
            Down s -> ( rendChildren Vert, moveChildren s Vert False )
            Left s -> ( rendChildren Hori, moveChildren s Hori True )
            Right s -> ( rendChildren Hori, moveChildren s Hori False )
            In -> ( rendStackChildren, moveStackChildren True )
            Out -> ( rendStackChildren, moveStackChildren False )
        -- ( children, childrenSize ) : ( List Rend, MaybeSizes )
        ( children, childrenSize ) = 
            renderChildrenFn sceneSize 
                -- children size reduced by border size
                ( maybeSize |> tupleMapEach ( Maybe.map ( \w -> w - rb - lb ) )
                    ( Maybe.map ( \h -> h - tb - bb ) ) )
                def.children
        ( width, height ) = tupleMap2 getSize maybeSize childrenSize
        border = case def.border of
            Just bs -> renderRectBorder bs width height
            _ -> []
        bgs = List.map ( \bg -> renderRectBg bg width height ) def.bgs
        movedCs : List ( RendInto Form )
        movedCs = moveChildrenFn ( width, height ) ( tb, rb, bb, lb ) 
            children
        rend cs' = collage' width height cs'
        cPopups = List.concatMap ( \c -> c.popups ) movedCs
        cRelatives = List.concatMap ( \c -> c.relatives ) movedCs
        rendAsRoot r = rendChild sceneSize ( tupleMap Just sceneSize ) r
        rendPopups = List.map rendAsRoot def.popups
        popups = List.map ( \p -> toForm p.element ) rendPopups
        pPopups = List.concatMap ( \p -> p.popups ) rendPopups
        pRelatives = List.concatMap ( \p -> p.relatives ) rendPopups
        alignRelatives r = 
                let ( w, h ) = tupleMap toFloat ( sizeOf r.element )
                in  move' ( -w * 0.5, -h * 0.5 ) ( toForm r.element )
        rendRelatives = List.map rendAsRoot def.relatives
        relatives = List.map alignRelatives rendRelatives
        rPopups = List.concatMap ( \r -> r.popups ) rendRelatives
        rRelatives = List.concatMap ( \r -> r.relatives ) rendRelatives
        -- relatives of popups?
        cs = List.map ( \c -> c.element ) movedCs
        rendered = rend ( bgs ++ border ++ cs )
        moveRelatives = if isRoot 
            then List.map ( move' ( width * 0.5, height * 0.5 ) ) 
            else identity 
    in  { element = rendered
        , relatives = moveRelatives 
            ( pRelatives ++ rRelatives ++ cRelatives ++ relatives )
        , popups = pPopups ++ rPopups ++ cPopups ++ popups
        }

tupleMapEach : ( a -> b ) -> ( c -> d ) -> ( a, c ) -> ( b, d )
tupleMapEach f g ( x, y ) = ( f x, g y )

renderRectBorder : BorderStyle -> Size -> Size -> List Form
renderRectBorder bs width height = 
    let ( width', height' ) = ( ceiling width, ceiling height )
        rendSame size = [ ( Graphics.Collage.outlined 
            { defaultLine | color = bs.color, width = size * 2.0 } 
            ( renderRect width height ) ) ]
        halfW = width * 0.5
        halfH = height * 0.5
        rendBorder size start end = Graphics.Collage.traced
            { defaultLine | color = bs.color, width = size * 2.0 }
            ( Graphics.Collage.segment start end )
        rendDiff t r b l = 
            [ rendBorder r ( halfW, halfH ) ( halfW, -halfH )
            , rendBorder l ( -halfW, -halfH ) ( -halfW, halfH )
            , rendBorder t ( -halfW, halfH ) ( halfW, halfH )
            , rendBorder b ( halfW, -halfH ) ( -halfW, -halfH ) ]
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

rendChildren : Side -> Sizes -> MaybeSizes -> List Node 
   -> ( List Rend, MaybeSizes )
rendChildren side sceneSize parentSize nodes =
    let ( fills, fixesAndFits ) = 
            List.partition ( ( onWhich side ) |> extentIsFill ) nodes
        fillCount = fills |> List.length
        adjOtherParentSize = 
            recalcParentSize side sceneSize parentSize fixesAndFits fillCount
        adjParentSize = case side of
            Hori -> ( adjOtherParentSize, snd parentSize )
            Vert -> ( fst parentSize, adjOtherParentSize )
        children = List.map ( rendChild sceneSize adjParentSize ) nodes
        childrenSize = List.map ( \c -> sizeOf c.element ) children
        childrenW = List.map fst childrenSize |> List.maximum 
            |> Maybe.map toFloat
        childrenH = List.map snd childrenSize |> List.sum |> toFloat |> Just
    in ( children, ( childrenW, childrenH ) )
        
type Side = Vert | Hori

onWhich : Side -> Which a
onWhich side = case side of
        Hori -> fst
        Vert -> snd

rendStackChildren : Sizes -> MaybeSizes -> List Node ->
   ( List Rend, MaybeSizes )
rendStackChildren sceneSize ( parentW, parentH ) nodes =
    let children = 
            List.map ( rendChild sceneSize ( parentW, parentH ) ) nodes
        childrenSize = List.map ( \c -> sizeOf c.element ) children
        childrenW = List.map fst childrenSize |> List.maximum
            |> Maybe.map toFloat
        childrenH = List.map snd childrenSize |> List.maximum 
            |> Maybe.map toFloat
    in ( children, ( childrenW, childrenH ) )

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
                renderFits = 
                    List.map ( \fit -> 
                        let f = rendChild sceneSize parentSize fit 
                        in  f.element ) fits
                fitsSize = List.foldr (+) 0.0 
                    <| List.map ( sizeOf >> ( onWhich side ) >> toFloat ) 
                        renderFits
                recalc size = ( size - fixesSize ) / ( toFloat fillCount )
            in Maybe.map ( \s -> if fillCount > 0 then recalc s else s ) 
                ( ( onWhich side ) parentSize )

rendChild : Sizes -> MaybeSizes -> Node -> Rend
rendChild sceneSize parentSize node =
     case node.nodeType of
        Rect def -> rendRect def sceneSize parentSize False
        Text def -> renderText def |> elemToRend

elemToRend : Element -> Rend
elemToRend e =
    { element = e
    , relatives = []
    , popups = []
    }
                
moveChildren : Spacing -> Side -> Bool -> ( Size, Size ) 
         -> ( Size, Size, Size, Size ) -> List Rend -> List ( RendInto Form )
moveChildren spacing side reverse ( parentW, parentH ) ( tb, rb, bb, lb ) 
    children = 
    let ( halfW, halfH ) = ( parentW * 0.5, parentH * 0.5 )
        moveChild : Rend -> Size -> ( RendInto Form, Size )
        moveChild child offset = 
            let c = child.element
                ( childW, childH ) = 
                    ( widthOf c |> toFloat , heightOf c |> toFloat )
                ( ( w, h ), nextOffset ) = case side of
                    Vert ->
                        ( ( -lb, offset - tb )
                        , offset - childH - spacing )
                    Hori ->
                        ( ( offset - lb, -tb )
                        , offset + childW + spacing )
                -- align at top left corner
                childPos = 
                    ( w + halfW - childW * 0.5, h + halfH - childH * 0.5 )
            in  (   { child 
                    | element = move' childPos ( toForm c )
                    , relatives = List.map ( move' ( w, h ) ) child.relatives
                    }
                , nextOffset )
        fold = case reverse of
            True -> List.foldr
            False -> List.foldl
    in  fold ( \x acc -> 
            ( moveChild x 
                -- get the offset from the previous child if any
                ( List.head acc |> Maybe.map snd |> Maybe.withDefault 0.0 ) 
            ) :: acc ) 
            [] children 
                -- throw away the offsets
                |> List.map fst

moveStackChildren : Bool -> ( Size, Size ) -> ( Size, Size, Size, Size ) 
         -> List Rend -> List ( RendInto Form )
moveStackChildren reverse ( parentW, parentH ) ( tb, rb, bb, lb ) children = 
    let ( halfW, halfH ) = ( parentW * 0.5, parentH * 0.5 )
        moveChild : Rend -> RendInto Form
        moveChild child = 
            let c = child.element
                ( childW, childH ) =
                        ( widthOf c |> toFloat, heightOf c |> toFloat )
                childPos = 
                    ( -lb + halfW - childW * 0.5, -tb + halfH - childH * 0.5 )
            in  { child 
                | element = move' childPos ( toForm c )
                , relatives = List.map ( move' ( -lb, -tb ) ) child.relatives 
                }
        cs = case reverse of
            True -> List.reverse children
            False -> children
    in List.map moveChild cs

renderText : TextDef -> Element
renderText def = fromString def.text |> leftAligned
