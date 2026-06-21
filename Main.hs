import System.IO
import Control.Monad

import Parser (parseInstance)
import Game (estatInicial, juga, Mode(..))

import Utils (maybeHead)

main :: IO()
main = do
    putStr "Instance path: "
    ins <- parseInstance <$> (getLine >>= readFile)
    -- I kept this for testing purposes, remove it if you want to 
    putStrLn $ show ins

    putStrLn "Select gamemode."
    putStrLn "(0: Manual, 1: Automatic solution)"
    modeInput <- getLine
    let mode = case modeInput of
            "1" -> AI
            _ -> Huma
    putStrLn $ "Selected " ++ show mode ++ " gamemode."
    -- See 'estatInicial' for more context
    let (config, estat) = estatInicial ins
    juga mode config estat