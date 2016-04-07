import Node     exposing ( .. )
import Render   exposing ( .. )
import Color    exposing ( .. )

import Graphics.Input.Field exposing ( Content, noContent )
import Signal               exposing ( .. )
import String               exposing ( left )
import Text                 exposing ( .. )

actionMB : Mailbox Content
actionMB = mailbox noContent

actionOptions = [ "change", "delete", "insert" ]

insertTypeMB : Mailbox Content
insertTypeMB = mailbox noContent

insertOptions = [ "rectangle", "text" ]

type SusState
    = Ambiguous
    | Match String

matchCounts : Signal Content -> List String -> Signal SusState
matchCounts c options = Signal.map ( \c ->
    if String.isEmpty c.string
    then Ambiguous
    else case List.filter ( String.startsWith c.string ) options of
        x :: [ ] -> Match x
        _ -> Ambiguous
    ) c

actionMatchCounts = matchCounts actionMB.signal actionOptions
afterActions = Signal.map2 ( \mc insertType -> case mc of
        Ambiguous -> [ ]
        Match "insert" -> 
            [ 
                { nodeType = SUS
                    { name = "node type"
                    , extents = ( Fix 200.0, Fix 25.0 )
                    , address = insertTypeMB.address
                    , content = insertType
                    , options = insertOptions
                    }
                , status = Enabled
                }
            ] 
        Match _ -> [ ]
    ) actionMatchCounts insertTypeMB.signal

sus = Signal.map2 ( \action afterAction -> 
    { nodeType = Rect
        { rectDef
        | extents = ( Fill 1.0, Fill 1.0 )
        , border = Nothing
        , children = [
            { nodeType = Rect
                { rectDef
                | extents = ( Fill 1.0, Fix 27.0 )
                , dir = Right 0.0
                , children = [
                    { nodeType = SUS
                        { name = "action"
                        , extents = ( Fix 200.0, Fix 25.0 )
                        , address = actionMB.address
                        , content = action
                        , options = actionOptions
                        }
                    , status = Enabled
                    } ] ++ afterAction
                }
            , status = Enabled
            } ]
        }
    , status = Enabled
    }
    ) actionMB.signal afterActions

scene = Signal.map ( \action ->
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
                            { nodeType = Text ( textDef "3" )
                            , status = Enabled
                            }
                        ]
                    , popups = []
                    , relatives = []
                    }
                , status = Enabled
                }
                , { nodeType = InputText
                    { name = "Input Text"
                    , handler = Signal.message actionMB.address
                    , content = action 
                    }
                , status = Enabled
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
                                        { nodeType = Text ( textDef "1" )
                                        , status = Enabled
                                        }
                                    ]
                                , popups = []
                                , relatives = []
                                }
                            , status = Enabled
                            }
                        ]
                    , relatives =
                        [
                            ( { nodeType = Rect
                                { extents = ( Fix 100.0, Fix 40.0 )
                                , dir = Down 0.0
                                , border = Just { thickness = All 5.0
                                    , color = Color.blue }
                                , bgs = [ Filled Color.green ]
                                , children = 
                                    [
                                        { nodeType = Text ( textDef "2" )
                                        , status = Enabled
                                        }
                                    ]
                                , popups = []
                                , relatives = []
                                } 
                            , status = Enabled
                            }, ( 0.0, 0.0 ) )
                        ]
                    }
                , status = Enabled
                }
            ]
        , popups = [ ]
        , relatives = [ ]
    }
    , status = Enabled
    } ) actionMB.signal

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
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
