import Node     exposing ( .. )
import Render   exposing ( .. )
import Color    exposing ( .. )

import Graphics.Input.Field exposing ( Content, noContent )
import Signal               exposing ( .. )
import String               exposing ( left )
import Text                 exposing ( .. )

actionMB : Mailbox Content
actionMB = mailbox noContent

type SusState
    = Ambiguous
    | Match String

matchCount : List String -> Signal SusState
matchCount options = Signal.map ( \c ->
    if String.isEmpty c.string
    then Ambiguous
    else case List.filter ( String.startsWith c.string ) options of
        x :: [ ] -> Match x
        _ -> Ambiguous
    ) actionMB.signal

sus = 
    { nodeType = SUS
        { name = "action"
        , address = actionMB.address
        , content = actionMB.signal
        , options = [ "insert", "delete", "intern", "interpolation" ]
        }
    }

scene =
    { nodeType = Rect
        { extents = ( Fix 300.0, Fix 400.0 )
        , dir = Down 0.0
        , border = Just { thickness = TRBL 40.0 25.0 5.0 10.0
            , color = Color.red }
        , bgs = [ Gradient grad |> Signal.constant ]
        , children =
            [
                { nodeType = Rect
                    { extents = ( Fix 100.0, Fix 40.0 )
                    , dir = Down 0.0
                    , border = Just { thickness = All 5.0
                        , color = Color.blue }
                    , bgs = [ Filled Color.grey |> Signal.constant ]
                    , children = 
                        [
                            { nodeType = Text ( textDef "3" ) }
                        ]
                    , popups = []
                    , relatives = []
                    }
                }
                , { nodeType = InputText
                    { name = "Input Text"
                    , handler = Signal.message actionMB.address
                    , content = actionMB.signal
                    }
                }
                , { nodeType = Rect
                    { extents = ( Fix 200.0, Fix 200.0 )
                    , dir = Down 0.0
                    , border = Just { thickness = All 5.0
                        , color = Color.yellow }
                    , bgs = [ Filled Color.brown |> Signal.constant ]
                    , children = []
                    , popups = 
                        [
                            { nodeType = Rect
                                { extents = ( Fix 100.0, Fix 40.0 )
                                , dir = Down 0.0
                                , border = Just { thickness = All 5.0
                                    , color = Color.blue }
                                , bgs = [ Filled Color.grey |> Signal.constant ]
                                , children = 
                                    [
                                        { nodeType = Text ( textDef "1" ) }
                                    ]
                                , popups = []
                                , relatives = []
                                }
                            }
                        ]
                    , relatives =
                        [
                            ( { nodeType = Rect
                                { extents = ( Fix 100.0, Fix 40.0 )
                                , dir = Down 0.0
                                , border = Just { thickness = All 5.0
                                    , color = Color.blue }
                                , bgs = [ Filled Color.green |> Signal.constant ]
                                , children = 
                                    [
                                        { nodeType = Text ( textDef "2" ) }
                                    ]
                                , popups = []
                                , relatives = []
                                }
                            }, ( 0.0, 0.0 ) )
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

main = render sus
{-
TODO
 * input box
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
