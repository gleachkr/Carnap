{-#LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Carnap.Languages.PurePropositional.Logic.Gregory
    (parseGregorySD, parseGregorySDE, gregorySDCalc, gregorySDECalc
    , GregorySD(..), GregorySDE(..)) where

import Text.Parsec
import Carnap.Core.Data.Types (Form)
import Carnap.Languages.PurePropositional.Syntax
import Carnap.Languages.PurePropositional.Parser
import Carnap.Languages.PurePropositional.Util (dropOuterParens)
import Carnap.Calculi.Util
import Carnap.Calculi.NaturalDeduction.Syntax
import Carnap.Calculi.NaturalDeduction.Parser
import Carnap.Calculi.NaturalDeduction.Checker
import Carnap.Languages.PurePropositional.Logic.BergmannMoorAndNelson

newtype GregorySD = GregorySD { getGregorySD :: LogicBookSD }

newtype GregorySDE = GregorySDE { getGregorySDE :: LogicBookSDPlus }

instance Show GregorySD where
    show (GregorySD ConjIntro)  = "∧I"
    show (GregorySD ConjElim1)  = "∧E"
    show (GregorySD ConjElim2)  = "∧E"
    show (GregorySD CondIntro1) = "→I"
    show (GregorySD CondIntro2) = "→I"
    show (GregorySD CondElim)   = "→E"
    show (GregorySD NegeIntro1) = "¬I"
    show (GregorySD NegeIntro2) = "¬I"
    show (GregorySD NegeIntro3) = "¬I"
    show (GregorySD NegeIntro4) = "¬I"
    show (GregorySD NegeElim1)  = "¬E" 
    show (GregorySD NegeElim2)  = "¬E"
    show (GregorySD NegeElim3)  = "¬E"
    show (GregorySD NegeElim4)  = "¬E"
    show (GregorySD BicoIntro1) = "↔I"
    show (GregorySD BicoIntro2) = "↔I"
    show (GregorySD BicoIntro3) = "↔I"
    show (GregorySD BicoIntro4) = "↔I"
    show (GregorySD BicoElim1)  = "↔E"
    show (GregorySD BicoElim2)  = "↔E"
    show (GregorySD Reiterate)  = "↔R"
    show (GregorySD (Pr _))     = "P"
    show (GregorySD x) = show x

instance Show GregorySDE where
    show (GregorySDE (SD x)) = show (GregorySD x)
    show (GregorySDE x) = show x

instance Inference GregorySD PurePropLexicon (Form Bool) where
    ruleOf (GregorySD x) = ruleOf x

    indirectInference (GregorySD x) = indirectInference x 

    isAssumption (GregorySD x) = isAssumption x

    isPremise (GregorySD x) = isPremise x

    restriction (GregorySD x) = restriction x

instance Inference GregorySDE PurePropLexicon (Form Bool) where
    ruleOf (GregorySDE x) = ruleOf x

    indirectInference (GregorySDE x) = indirectInference x 

    isAssumption (GregorySDE x) = isAssumption x

    isPremise (GregorySDE x) = isPremise x

    restriction (GregorySDE x) = restriction x

parseGregorySD :: RuntimeDeductionConfig PurePropLexicon (Form Bool) -> Parsec String u [GregorySD]
parseGregorySD rtc = do r <- choice (map (try . string) ["Assumption" ,"&I","/\\I", "∧I","&E","/\\E","∧E","CI","=>I", "->I","→I", ">I", "⊃I","→E", "⊃E","CE","->E"
                                                          , "→E", ">E" ,"~I","-I", "¬I","~E","-E","¬E" ,"vI","\\/I","|I","∨I", "vE","\\/E", "|E", "∨E","BI","<=>I","<>I","<->I"
                                                          , "↔I", "BE", "<->E","<>E", "<=>E", "↔E", "A","P", "R"]) <|> ((++) <$> string "A/" <*> many anyChar)
                        let theRule = case r of
                               r | r `elem` ["A"] -> [AS ""]
                                 | r `elem` ["A/>I", "A/->I"] -> [AS "/⊃I"]
                                 | r `elem` ["A/=I"] -> [AS "/≡I"]
                                 | r `elem` ["P", "Assumption"] -> [Pr (problemPremises rtc)]
                                 | r `elem` ["&I","/\\I","∧I"] -> [ConjIntro]
                                 | r `elem` ["&E","/\\E","∧E"] -> [ConjElim1, ConjElim2]
                                 | r `elem` ["CI","->I", "=>I","→I",">I", "⊃I"] -> [CondIntro1,CondIntro2]
                                 | r `elem` ["CE","->E","→E",">E", "⊃E"] -> [CondElim]
                                 | r `elem` ["~I","¬I","-I"]  -> [NegeIntro1, NegeIntro2, NegeIntro3, NegeIntro4]
                                 | r `elem` ["~E","¬E","-E"]  -> [NegeElim1, NegeElim2, NegeElim3, NegeElim4]
                                 | r `elem` ["vI","\\/I","|I","∨I"] -> [DisjIntro1, DisjIntro2]
                                 | r `elem` ["vE","\\/E","|E","∨E"] -> [DisjElim1, DisjElim2,DisjElim3, DisjElim4]
                                 | r `elem` ["BI","<->I","<>I","<=>I","↔I"] -> [BicoIntro1, BicoIntro2, BicoIntro3, BicoIntro4]
                                 | r `elem` ["BE","<->E","<>E","<=>E","↔E"] -> [BicoElim1, BicoElim2]
                               'A':'/':rest -> [AS (" / " ++ rest)]
                               "R" -> [Reiterate]
                        return $ map GregorySD theRule

parseGregorySDE :: RuntimeDeductionConfig PurePropLexicon (Form Bool) -> Parsec String u [GregorySDE]
parseGregorySDE rtc = try (map (GregorySDE . SD . getGregorySD) <$> parseGregorySD rtc) <|> parsePlus
    where parsePlus = do r <- choice (map (try . string) ["MT","HS","DS","Com","Assoc","Impl", "DN", "DeM", "Idem", "Trans", "Exp", "Dist", "Equiv"])
                         let theRule = case r of
                                    r | r == "MT" -> [MT]
                                      | r == "HS" -> [HS]
                                      | r == "DS" -> [DS1,DS2]
                                      | r == "Com" -> [Com1,Com2]
                                      | r == "Assoc" -> [Assoc1,Assoc2,Assoc3,Assoc4]
                                      | r == "Impl" -> [Impl1,Impl2]
                                      | r == "DN" -> [DN1, DN2]
                                      | r == "DeM" -> [DeM1,DeM2, DeM3, DeM4]
                                      | r == "Idem" -> [Idem1, Idem2, Idem3, Idem4]
                                      | r == "Trans" -> [Trans1, Trans2]
                                      | r == "Exp" -> [Exp1, Exp2]
                                      | r == "Dist" -> [Dist1, Dist2, Dist3, Dist4]
                                      | r == "Equiv" -> [Equiv1, Equiv2, Equiv3, Equiv4]
                         return $ map GregorySDE theRule

parseGregorySDProof :: RuntimeDeductionConfig PurePropLexicon (Form Bool) -> String -> [DeductionLine GregorySD PurePropLexicon (Form Bool)]
parseGregorySDProof ders = toDeductionFitchAlt (parseGregorySD ders) (purePropFormulaParser gregoryOpts)

parseGregorySDEProof :: RuntimeDeductionConfig PurePropLexicon (Form Bool) -> String -> [DeductionLine GregorySDE PurePropLexicon (Form Bool)]
parseGregorySDEProof ders = toDeductionFitchAlt (parseGregorySDE ders) (purePropFormulaParser gregoryOpts)

gregorySDCalc = mkNDCalc 
    { ndRenderer = FitchStyle BergmanMooreAndNelsonStyle
    , ndParseProof = parseGregorySDProof
    , ndProcessLine = hoProcessLineFitch
    , ndProcessLineMemo = Just hoProcessLineFitchMemo
    , ndParseSeq = extendedPropSeqParser
    , ndParseForm = purePropFormulaParser extendedLetters
    , ndNotation = dropOuterParens
    }

gregorySDECalc = mkNDCalc 
    { ndRenderer = FitchStyle BergmanMooreAndNelsonStyle
    , ndParseProof = parseGregorySDEProof
    , ndProcessLine = hoProcessLineFitch
    , ndProcessLineMemo = Just hoProcessLineFitchMemo
    , ndParseSeq = extendedPropSeqParser
    , ndParseForm = purePropFormulaParser extendedLetters
    , ndNotation = dropOuterParens
    }
