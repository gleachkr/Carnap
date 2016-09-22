{-#LANGUAGE DeriveGeneric, OverloadedStrings#-}
module Carnap.GHCJS.SharedTypes (
        GHCJSCommand(..)
) where

import Prelude
import Data.Aeson
import Data.Either
import Text.Parsec (parse)
import GHC.Generics
import Carnap.Languages.PurePropositional.Logic (DerivedRule(..))
import Carnap.Languages.PurePropositional.Parser

data GHCJSCommand = EchoBack (String, Bool)
        | SubmitSyntaxCheck String
        | SubmitTranslation String
        | SubmitDerivation String String
        | SaveDerivedRule String DerivedRule
        | RequestDerivedRulesForUser
        deriving (Generic, Show)

instance ToJSON GHCJSCommand

instance FromJSON GHCJSCommand

instance ToJSON DerivedRule where
        toJSON (DerivedRule conclusion prems) = 
            object [ "conclusion" .= show conclusion
                   , "premises" .= map show prems]

instance FromJSON DerivedRule where
        parseJSON (Object v) = 
            do c  <- v .: "conclusion"
               ps <- v .: "premises"
               let ps' = map toForm ps 
               case (toForm c, lefts ps') of 
                 (Right f, []) -> return $ DerivedRule f (rights ps')
                 _ -> mempty
            where toForm = parse purePropFormulaParser ""
        parseJSON _ = mempty
