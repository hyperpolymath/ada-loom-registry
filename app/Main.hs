{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import System.Environment (getArgs)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.ByteString as B
import Data.Aeson (FromJSON, eitherDecodeStrict')
import Language.Nickel.Parser (parseNickelFile)
import Language.Nickel.Eval (eval)
import Language.Nickel.Types (NValue, nvalueToJSON)

-- Our Config type, now with tools to be created from JSON
data Config = Config { name :: Text, version :: Text }
  deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      -- Read the Nickel file content
      content <- B.readFile filename
      
      -- The process is: Parse -> Evaluate -> Convert to Haskell Type
      let result = do
            -- 1. Parse the file into a Nickel expression
            expr <- parseNickelFile filename content
            -- 2. Evaluate the expression to get a Nickel value
            evaluatedValue <- eval expr
            -- 3. Convert the Nickel value to a JSON representation
            let jsonValue = nvalueToJSON evaluatedValue
            -- 4. Use Aeson to decode the JSON into our Config type
            eitherDecodeStrict' jsonValue

      case result of
        Left err -> putStrLn $ "Error processing Nickel file: " ++ err
        Right config -> putStrLn $ "Successfully parsed config: " ++ show config

    _ -> putStrLn "Usage: spindle <path-to-config.ncl>"
