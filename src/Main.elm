import Node     exposing ( .. )
import Render   exposing ( .. )
import Color    exposing ( .. )

import Graphics.Input.Field exposing ( Content, noContent )

name : Signal.Mailbox Content
name = Signal.mailbox noContent

scene =
    { nodeType = Rect
        { extents = ( Fix 300.0, Fix 400.0 )
        , dir = Down 0.0
        , border = Just { thickness = TRBL 40.0 25.0 5.0 10.0
            , color = Color.red }
        , bgs = [ Gradient grad ]
        , children =
            [
                { nodeType = Rect
                    { extents = ( Fix 100.0, Fix 40.0 )
                    , dir = Down 0.0
                    , border = Just { thickness = All 5.0
                        , color = Color.blue }
                    , bgs = [ Filled Color.grey ]
                    , children = 
                        [
                            { nodeType = Text { text = "3" } }
                        ]
                    , popups = []
                    , relatives = []
                    }
                }
                , { nodeType = InputText
                    { name = "Input Text"
                    , handler = Signal.message name.address
                    , content = name.signal
                    }
                }
                , { nodeType = Rect
                    { extents = ( Fix 200.0, Fix 200.0 )
                    , dir = Down 0.0
                    , border = Just { thickness = All 5.0
                        , color = Color.yellow }
                    , bgs = [ Filled Color.brown ]
                    , children = []
                    , popups = 
                        [
                            { nodeType = Rect
                                { extents = ( Fix 100.0, Fix 40.0 )
                                , dir = Down 0.0
                                , border = Just { thickness = All 5.0
                                    , color = Color.blue }
                                , bgs = [ Filled Color.grey ]
                                , children = 
                                    [
                                        { nodeType = Text { text = "1" } }
                                    ]
                                , popups = []
                                , relatives = []
                                }
                            }
                        ]
                    , relatives =
                        [
                            { nodeType = Rect
                                { extents = ( Fix 100.0, Fix 40.0 )
                                , dir = Down 0.0
                                , border = Just { thickness = All 5.0
                                    , color = Color.blue }
                                , bgs = [ Filled Color.green ]
                                , children = 
                                    [
                                        { nodeType = Text { text = "2" } }
                                    ]
                                , popups = []
                                , relatives = []
                                }
                            }
                        ]
                    --, popups = []
                    --, relatives = []
                    }
                }
            ]
        , popups = [ ]
        , relatives = [ ]
        }
    }

grad : Gradient
grad = linear (0,60) (0,-60)
      [ (0, rgb 0 171 235)
      , (0.79, white)
      , (0.8, rgb 38 192 0)
      , (1, white)
      ]

main = render scene
{-
TODO
 * input box
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
