import WnHtml exposing (..)


testText : String -> Node
testText t =  
        { nodeType = Text { text = t }
        , id = 0
        , children = NoChildren }

scene : Scene
scene = makeScene [ testText "ikliosd", testText "adsf", testText "bdsfgd" ]

{-background (width, height) = 
        let bkgrnd = rect (toFloat width) (toFloat height) in
            collage width height [ filled black bkgrnd ]-}

--main = Signal.map background Window.dimensions
main = renderScene scene
