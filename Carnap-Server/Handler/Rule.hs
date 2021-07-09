module Handler.Rule where

import           Import

import           Carnap.GHCJS.SharedTypes
import qualified Data.CaseInsensitive        as CI
import qualified Data.Text.Encoding          as TE
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Html5.Attributes
import           Text.Hamlet                 (hamletFile)
import           Text.Julius                 (juliusFile)
import           TH.RelativePaths            (pathRelativeToCabalPackage)

import           Util.Database
import           Util.Handler                (addDocScripts)

getRuleR :: Handler Html
getRuleR = do derivedPropRules <- getPropDrList
              derivedFOLRules <- getFOLDrList
              ruleLayout $ [whamlet|
                            <div.container>
                                <h1> Index of Basic Propositional Rules
                                <table.rules>
                                    <thead> <th> Name </th> <th> Premises </th><th> Conclusion </th>
                                    <tbody>
                                        <tr> <td> MP  </td> <td> φ, φ→ψ </td> <td> ψ </td
                                        <tr> <td> MT  </td> <td> ¬ψ, φ→ψ </td> <td> ¬φ </td>
                                        <tr> <td> DNE </td> <td> ¬¬φ </td> <td> φ </td>
                                        <tr> <td> DNI </td> <td> φ </td> <td> ¬¬φ </td>
                                        <tr> <td> S   </td> <td> φ∧ψ </td> <td> φ </td>
                                        <tr> <td> S   </td> <td> φ∧ψ </td> <td> ψ </td>
                                        <tr> <td> ADJ </td> <td> φ, ψ </td> <td> φ∧ψ </td>
                                        <tr> <td> MTP </td> <td> φ∨ψ, ¬φ </td> <td> ψ </td>
                                        <tr> <td> MTP </td> <td> φ∨ψ, ¬ψ </td> <td> φ </td>
                                        <tr> <td> ADD </td> <td> φ </td> <td> φ∨ψ </td>
                                        <tr> <td> ADD </td> <td> ψ </td> <td> φ∨ψ </td>
                                        <tr> <td> BC  </td> <td> ψ↔φ </td> <td> ψ→φ  </td>
                                        <tr> <td> BC  </td> <td> ψ↔φ </td> <td> φ→ψ  </td>
                                        <tr> <td> CB  </td> <td> ψ→φ, φ→ψ  </td> <td> φ↔ψ </td>
                                <h1> Index of Basic Predicate Rules
                                <table.rules>
                                    <thead> <th> Name </th> <th> Premises </th><th> Conclusion </th>
                                    <tbody>
                                        <tr> <td> UI </td> <td> ∀xφ<sub>x</sub> </td> <td> φ<sub>x</sub>(c) </td
                                        <tr> <td> EG </td> <td> φ<sub>x</sub>(c) </td> <td> ∃xφ<sub>x</sub> </td>
                                <h1> Index of Derived Rules
                                $maybe propRules <- derivedPropRules
                                    <div.derivedRules>
                                        <h2> Propositional Rules
                                        <table.rules>
                                            <thead>
                                                <tr>
                                                    <th>Name
                                                    <th>Premises
                                                    <th>Conclusion
                                            <tbody>
                                                $forall rule <- propRules
                                                    #{rule}
                                $nothing
                                <div.ruleBuilder>
                                    <h2>The Propositional Rule Builder
                                    <div data-carnap-type="proofchecker"
                                         data-carnap-system="prop"
                                         data-carnap-guides="montague"
                                         data-carnap-options="resize"
                                         data-carnap-submission="saveRule">

                                $maybe folRules <- derivedFOLRules
                                    <div.derivedRules>
                                        <h2> First-Order Rules
                                        #{folRules}
                                $nothing
                                    <hr.hrSep>
                                <div.ruleBuilder>
                                    <h2>The First-Order Rule Builder
                                    <div data-carnap-type="proofchecker"
                                         data-carnap-system="firstOrder"
                                         data-carnap-guides="montague"
                                         data-carnap-options="resize"
                                         data-carnap-submission="saveRule">
                            |]

ruleLayout :: ToWidget App a => a -> HandlerFor App Html
ruleLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        authmaybe <- maybeAuth
        mud <- maybeUserData
        mcourse <- maybeUserCourse
        mdoc <- maybeUserTextbookDoc
        let isInstructor = not $ null (mud >>= userDataInstructorId . entityVal)
        pc <- widgetToPageContent $ do
            toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/command.julius")

            addDocScripts
            addStylesheet $ StaticR css_tufte_css
            addStylesheet $ StaticR css_tuftextra_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile =<< pathRelativeToCabalPackage "templates/default-layout-wrapper.hamlet")

getPropDrList :: HandlerFor App (Maybe [Html])
getPropDrList = do maybeCurrentUserId <- maybeAuthId
                   case maybeCurrentUserId of
                       Nothing -> return Nothing
                       Just uid -> do savedRulesOld <- getDerivedRules uid
                                      savedRules <- getRules uid
                                      return $ Just $ formatOldPropRules savedRulesOld ++ formatPropRules savedRules

getFOLDrList :: HandlerFor App (Maybe Html)
getFOLDrList = do maybeCurrentUserId <- maybeAuthId
                  case maybeCurrentUserId of
                       Nothing  -> return Nothing
                       Just uid -> Just . formatFOLRules <$> getRules uid


formatOldPropRules :: Functor f => f SavedDerivedRule -> f Html
formatOldPropRules rules = map toRow rules
    where toRow (SavedDerivedRule dr n _ _) = let (Just dr') = decodeRule dr in
                                              B.tr $ do B.td $ B.toHtml $ "D-" ++ n
                                                        B.td $ B.toHtml $ intercalate "," $ map show $ premises dr'
                                                        B.td $ B.toHtml $ show $ conclusion dr'

formatPropRules :: Functor f => f SavedRule -> f Html
formatPropRules rules = map toRow rules
    where toRow (SavedRule (PropRule dr) n _ _) = B.tr $ do B.td $ B.toHtml $ "D-" ++ n
                                                            B.td $ B.toHtml $ intercalate "," $ map show $ premises dr
                                                            B.td $ B.toHtml $ show $ conclusion dr
          toRow _ = return ()

formatFOLRules :: (MonoFoldable mono, Element mono ~ SavedRule) => mono -> Html
formatFOLRules rules = B.table B.! class_ "rules" $ do
        B.thead $ do
            B.th "Name"
            B.th "Premises"
            B.th "Conclusion"
        B.tbody $ mapM_ toRow rules
    where toRow (SavedRule (FOLRule dr) n _ _) = B.tr $ do B.td $ B.toHtml $ "D-" ++ n
                                                           B.td $ B.toHtml $ intercalate "," $ map show $ premises dr
                                                           B.td $ B.toHtml $ show $ conclusion dr
          toRow _ = return ()
