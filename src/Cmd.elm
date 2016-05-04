module Cmd
    (Cmd       (..)
    ,CmdCommon
    ,SusCmd
    ,SusState  (..)
    ,FloatCmd
    ,toSus
    ,cmdShorts
    ) where

import String               exposing (left)

type Cmd a
    = Sus (SusState a) (List (SusCmd a))
    | Float' (FloatCmd a)

type alias CmdCommon a = 
    {after : String
    ,next : Maybe (Cmd a)
    }

type alias SusCmd a =
    {name : String
    ,short : String
    ,afterShort : String
    ,cmdState : a
    ,common : CmdCommon a
    }

type SusState a
    = Ambiguous String
    | Match (SusCmd a)

type alias FloatCmd a = 
    {value : String
    ,common : CmdCommon a
    }

toSus : List (SusCmdDef a) -> Cmd a
toSus os = Sus ambiguous <| List.map (\o -> 
        let otherOs = List.filter (\o' -> o'.name /= o.name) os
        in toSusCmd 1 o otherOs) os

cmdShorts : List (SusCmd a) -> List String 
cmdShorts cmds = List.map (\cmd -> cmd.short) cmds

-- INTERNAL

type alias SusCmdDef a =
    {name : String
    ,cmdState : a
    ,common : CmdCommon a
    }

toSusCmd : Int -> SusCmdDef a -> List (SusCmdDef a) -> SusCmd a
toSusCmd chars o os =
    let short = String.left chars o.name
        isUnique = List.all (\o -> String.left chars o.name /= short) os
        afterShort = String.right (String.length o.name - chars) o.name
    in if isUnique 
        then 
            {name = o.name
            ,short = short
            ,afterShort = afterShort
            ,cmdState = o.cmdState
            ,common = o.common
            }
        else toSusCmd (chars + 1) o os

ambiguous = Ambiguous ""

