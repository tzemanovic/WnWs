import WnHtml exposing (..)
import Color exposing (lightOrange)
import Window


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

main = Signal.map (renderScene scene) Window.dimensions
