{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api
import Data.String
import Ledger
import OnChain.GOADAPolicy (serialisedScript)
import System.Environment (getArgs)
import Prelude

createPlutusScript :: PubKeyHash -> POSIXTime -> String -> IO ()
createPlutusScript pkh deadline filename = do
  result <- writeFileTextEnvelope filename Nothing (serialisedScript pkh deadline)
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> do
      putStrLn $ "Created the plutus policy script => " ++ filename
      return ()

main :: IO ()
main = do
  args <- getArgs
  let argsLen = length args
  let pkh = if argsLen > 0 then head args else error "Provide the Public Key Hash of the owner"
  let strdate = if argsLen > 1 then args !! 1 else error "Provide the deadline in posix time"
  let filename = if argsLen > 2 then args !! 2 else "minting-policy-" ++ pkh ++ "-" ++ strdate ++ ".plutus"
  let intdate = read strdate :: Integer
  putStrLn "Creating the plutus policy script"
  createPlutusScript (fromString pkh) (fromInteger intdate) filename
