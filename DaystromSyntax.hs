module DaystromSyntax where

type Name = String
type Stringlit = String
type Value = Integer

data Program = DaystromProgram [Alias] [Function]
        deriving (Show)

data Alias = StationAlias Name Name
        deriving (Show)

data Function = HelperFunction Name [Command] | MainFunction [Command]
        deriving (Show)

data Command = Com Name Instruction
        deriving (Show)

data Instruction = PushNum Value
                 | PushVar Name
                 | PopVar  Name
                 | StartOutputStandard
                 | StartOutputError
                 | PrintString String
                 | PrintVal
                 | EndOutput
                 | GetValue
                 | Swap
                 | Add
                 | Subtract
                 | Multiply
                 | IntDivide
                 | ExecuteFunction Name
                 | UntilNN Name Name [Command]
                 | UntilNV Name Value [Command]
                 | IfNN Name Name [Command] [Command]
                 | IfNV Name Value [Command] [Command]
        deriving (Show)
