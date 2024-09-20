{-# LANGUAGE MultiWayIf #-}

module CompilerOpts where

import Data.List as DL
import System.Console.GetOpt as SCG
import System.IO
import System.Exit as SX

data Flag
    = Version               -- -V
    | Help                  -- --help
    deriving (Eq,Ord,Show)

options :: [OptDescr Flag]
options =
    [ Option ['V','?'] ["version"]             (NoArg Version)                "Show version number.",
      Option []        ["help"]                (NoArg Help)                   "Print this help message."
    ]

compilerOpts :: [String] -> IO ([Flag],[String])
compilerOpts argv =
    case getOpt Permute options argv of
        (args,file,[]) ->
            if | DL.elem Help args ->
               do hPutStrLn stderr (greeting ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith SX.ExitSuccess
               | DL.elem Version args ->
               do hPutStrLn stderr (version ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith SX.ExitSuccess
               | DL.length file /= 2 ->
               do hPutStrLn stderr (flerror ++ greeting ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | otherwise -> return (DL.nub args, file)
        (_,_,errors) -> do
            hPutStrLn stderr (DL.concat errors ++ "\n" ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where
            greeting       = "Servant Image Server, Copyright (c) 2024 Matthew Mosior.\n"
            header         = "Usage: sis [-V] [apikey] [apisecret]\n"
            version        = "Servant-Image-Server, Version 0.1.0.0.\n"
            github         = "Please see https://github.com/Matthew-Mosior/Basic-Variant-Parser for more information.\n"
            flerror        = "Please provide your imagga api key and api secret.\n"
