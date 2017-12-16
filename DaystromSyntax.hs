module DaystromSyntax where

type Name = String

data Program = DaystromProgram [Alias] [Function]
