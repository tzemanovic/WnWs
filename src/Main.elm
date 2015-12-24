import WnHtml exposing ( .. )
import Color exposing ( .. )
import Window
import Graphics.Element exposing ( .. )


testText : String -> Node
testText t =  
    { nodeType = Text { text = t }
    , extents = ( Fix 50.0, Fit )
    , id = 0
    , children = Empty
    }

testRect : List Node -> Node
testRect children =
    { nodeType = Rect { background = lightOrange }
    , extents = ( Fit, Fit )
    , id = 0
    , children = Flow down children
    }

testRect2 children =
    { nodeType = Rect { background = lightBlue }
    , extents = ( Fix 10.0, Fix 20.0 )
    , id = 0
    , children = Flow outward children
    }

scene : Scene
scene = makeScene 
    [ testRect 
        [ testRect2 []
        , testText "gliosdsadfsr  sadf asdf sa"
        , testText "adsf"
        , testText "bdsfgd"
        ]
    ]

main = render scene

--TODO
-- add padding, border and margin
-- children outside their parent have to be popup
