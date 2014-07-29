-----------------------------------------------------------------------------------------------------------------------

module Options(options, Flag(..)) where

import System.Console.GetOpt

-----------------------------------------------------------------------------------------------------------------------

data Flag 
   = FlagFlatten
   | FlagReverse
   | FlagSimplify
   | FlagInfo
   deriving(Show, Eq)
   
   
optionDefs :: [OptDescr Flag]
optionDefs = 
   [
      Option "f" ["flatten"] (NoArg FlagFlatten) "flatten",
      Option "r" ["reverse"] (NoArg FlagReverse) "reverse",
      Option "s" ["simplify"] (NoArg FlagSimplify) "simplify",
      Option "i" ["info"] (NoArg FlagInfo) "info"
   ]


options :: [String] -> ([Flag], String)
options args = 
   case getOpt Permute optionDefs args of
      (o, file : _, []) -> (o, file)
      _                   -> error $ usageInfo header optionDefs
   where 
      header = "Usage: GpxTool [OPTION...] file"



-----------------------------------------------------------------------------------------------------------------------