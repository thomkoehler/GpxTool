-----------------------------------------------------------------------------------------------------------------------

module Options(options, Flag(..)) where

import System.Console.GetOpt

-----------------------------------------------------------------------------------------------------------------------

data Flag 
   = Flatten
   | Reverse
   | Info
   deriving(Show, Eq)
   
   
optionDefs :: [OptDescr Flag]
optionDefs = 
   [
      Option "f" ["flatten"] (NoArg Flatten) "flatten",
      Option "r" ["reverse"] (NoArg Reverse) "reverse",
      Option "i" ["info"] (NoArg Info) "info"
   ]


options :: [String] -> ([Flag], String)
options args = 
   case getOpt Permute optionDefs args of
      (o, (file : _), []) -> (o, file)
      _                   -> error $ usageInfo header optionDefs
   where 
      header = "Usage: GpxTool [OPTION...] file"



-----------------------------------------------------------------------------------------------------------------------