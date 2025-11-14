import Parser
import TypeInference

import Data.Map.Lazy (toList)
import Data.List (intercalate)

go :: IO ()
go = do
  let fileName = "src"
  src <- readFile fileName
  case parseEntry fileName src of
    Left err -> print err
    Right succ -> case runInferAll succ of
      Just as -> putStrLn $ intercalate "\n" $ map joinTup $ toList as
      other -> print other
  where
    joinTup (a, b) = show a ++ " : " ++ show b
