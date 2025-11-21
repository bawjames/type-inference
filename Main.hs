import Parser
import AlgorithmW

import Data.Map.Lazy (toList, empty)
import Data.List (intercalate)

printEnvFromFile :: FilePath -> IO ()
printEnvFromFile fileName = do
  src <- readFile fileName
  case parseEntry fileName src of
    Left err -> print err
    Right succ -> case runVarSupply $ inferAll empty succ of
      Left err -> putStrLn err
      Right succ -> putStrLn $ intercalate "\n" $ map pretty $ toList succ
  where
    pretty (a, b) = a ++ " : " ++ show b
