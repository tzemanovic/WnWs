import WnHtml exposing (..)
import Color exposing (lightOrange)


testText : String -> Node
testText t =  
        { nodeType = Text { text = t }
        , id = 0
        , children = Children []
        }

testRect : List Node -> Node
testRect children =
        { nodeType = Rect { width = 400, height = 200, background = lightOrange }
        , id = 0
        , children = Children children
        }

scene : Scene
scene = makeScene 
        [ testRect 
        [ testText "ikliosd"
        , testText "adsf"
        , testText "bdsfgd"
        ]
        ]

{-background (width, height) = 
        let bkgrnd = rect (toFloat width) (toFloat height) in
            collage width height [ filled black bkgrnd ]-}

--main = Signal.map background Window.dimensions
main = renderScene scene
