module Tiles.Utils.Terminal where
import           System.Process (callCommand)

data Color =
    Red
  | Green
  | Blue
  | Yellow
  | White
  | Black
  | Gray
  deriving (Eq, Show)

colorPrint :: Color -> String -> IO ()
colorPrint Red    = putStrLn . colorString Red
colorPrint Green  = putStrLn . colorString Green
colorPrint Blue   = putStrLn . colorString Blue
colorPrint Yellow = putStrLn . colorString Yellow
colorPrint White  = putStrLn . colorString White
colorPrint Black  = putStrLn . colorString Black
colorPrint Gray   = putStrLn . colorString Gray

colorString :: Color -> String -> String
colorString Red str    = "\ESC[31m" <> str <> "\ESC[0m"
colorString Green str  = "\ESC[32m" <> str <> "\ESC[0m"
colorString Blue str   = "\ESC[34m" <> str <> "\ESC[0m"
colorString Yellow str = "\ESC[33m" <> str <> "\ESC[0m"
colorString White str  = "\ESC[37m" <> str <> "\ESC[0m"
colorString Black str  = "\ESC[30m" <> str <> "\ESC[0m"
colorString Gray str   = "\ESC[90m" <> str <> "\ESC[0m"

colorPrintLn :: Color -> String -> IO ()
colorPrintLn color str = colorPrint color str >> putStrLn ""

clearScreen :: IO ()
clearScreen = callCommand "clear"

newline :: IO ()
newline = putStrLn ""
