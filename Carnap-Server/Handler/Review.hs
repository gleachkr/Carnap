{-# LANGUAGE DeriveGeneric #-}
module Handler.Review (getReviewR, putReviewR, deleteReviewR) where

import           Import

import           Data.List                                        (nub)
import           Data.Map                                         as M (fromList)
import           Data.Tree
import           Filter.Util                                      (sanitizeHtml)
import           Text.Blaze.Html5                                 (Markup)
import           Text.Read                                        (readMaybe)
import           Yesod.Form.Bootstrap3

import           Carnap.Calculi.NaturalDeduction.Syntax           (NaturalDeductionCalc (..))
import           Carnap.GHCJS.SharedFunctions                     (inOpts,
                                                                   simpleCipher,
                                                                   simpleHash)
import           Carnap.GHCJS.SharedTypes
import           Carnap.Languages.DefiniteDescription.Logic.Gamut (ofDefiniteDescSys)
import           Carnap.Languages.ModalPropositional.Logic        (ofModalPropSys)
import           Carnap.Languages.PureFirstOrder.Logic            (ofFOLSys)
import           Carnap.Languages.PureFirstOrder.Syntax
import           Carnap.Languages.PurePropositional.Logic         (ofPropSys)
import           Carnap.Languages.PurePropositional.Syntax
import           Carnap.Languages.PureSecondOrder.Logic           (ofSecondOrderSys)
import           Carnap.Languages.SetTheory.Logic                 (ofSetTheorySys)
import           Util.Assignment
import           Util.Data
import           Util.Handler                                     (addDocScripts)

deleteReviewR :: Text -> Text -> Handler Value
deleteReviewR _coursetitle _asgntitle = do
        msg <- requireCheckJsonBody :: Handler ReviewDelete
        case msg of
            DeleteProblem key -> do
                runDB $ delete key
                returnJson ("Problem deleted." :: Text)

putReviewR :: Text -> Text -> Handler Value
putReviewR coursetitle asgntitle =
        do Entity key _val <- getAssignmentByCourse coursetitle asgntitle
           ((theUpdate,_),_) <- runFormPost (identifyForm "updateSubmission" $ updateSubmissionForm Nothing "" "")
           case theUpdate of
               FormSuccess (ident, serializeduid, extra) -> do
                   success <- runDB $ do case readMaybe serializeduid of
                                               Just uid -> do msub <- getBy (UniqueProblemSubmission ident uid (Assignment (show key)))
                                                              case msub of
                                                                   Just (Entity k _) -> update k [ProblemSubmissionExtra =. Just extra] >> return True
                                                                   Nothing -> return False
                                               Nothing -> return False
                   if success then returnJson ("success" :: Text) else returnJson ("error: no submission record" :: Text)
               FormMissing -> returnJson ("no form" :: Text)
               (FormFailure s) -> returnJson ("error:" <> concat s :: Text)

getReviewR :: Text -> Text -> Handler Html
getReviewR coursetitle asgntitle =
        do Entity key val <- getAssignmentByCourse coursetitle asgntitle
           unsortedProblems <- runDB $ selectList [ProblemSubmissionAssignmentId ==. Just key] []
           uidAndData <- runDB $ do let uids = nub $ map (problemSubmissionUserId . entityVal) unsortedProblems
                                    muserdata <- mapM (getBy . UniqueUserData) uids
                                    return (sortBy maybeLnSort $ zip uids muserdata )
           let problems = sortBy theSorting unsortedProblems
               due = assignmentMetadataDuedate val
           defaultLayout $ do
               addDocScripts
               $(widgetFile "review")
               addScript $ StaticR ghcjs_allactions_runmain_js
    where maybeLnSort (_,Nothing) (_,Nothing) = EQ
          maybeLnSort (_,Nothing) _ = LT
          maybeLnSort _ (_,Nothing) = GT
          maybeLnSort (_,Just ud) (_,Just ud') = compare (userDataLastName $ entityVal ud)
                                                         (userDataLastName $ entityVal ud')
          theSorting p p' = scompare s s'
              where s = unpack . problemSubmissionIdent . entityVal $ p
                    s' = unpack . problemSubmissionIdent . entityVal $ p'
                    scompare a a' = case (break (== '.') a, break (== '.') a')  of
                                      ((h,[]),(h',[])) | compare (length h) (length h') /= EQ -> compare (length h) (length h')
                                                       | compare h h' /= EQ -> compare h h'
                                                       | otherwise -> EQ
                                      ((h,t), (h',t')) | compare (length h) (length h') /= EQ -> compare (length h) (length h')
                                                       | compare h h' /= EQ -> compare h h'
                                                       | otherwise -> scompare (drop 1 t) (drop 1 t')

selectUser :: (Foldable t, Show a) => t (a, Maybe (Entity UserData)) -> WidgetFor site ()
selectUser list =
        [whamlet|
            <select#selectStudent class="form-control">
                <option value="all">All Students
                $forall (k,v) <- list
                    $maybe v' <- v
                        <option value="#{show k}">
                            #{userDataLastName (entityVal v')}, #
                            #{userDataFirstName (entityVal v')}
                    $nothing
                        <option value="#{show v}">unknown
        |]

renderProblem
    :: (IsMap map, MapValue map ~ Maybe (Entity UserData), ContainerKey map ~ Key User)
    => Maybe UTCTime
    -> map
    -> Entity ProblemSubmission
    -> WidgetFor App ()
renderProblem due uidAndData (Entity key val) = do
        let ident = problemSubmissionIdent val
            uid = problemSubmissionUserId val
            extra = problemSubmissionExtra val
            correct = problemSubmissionCorrect val
            Just userdata = lookup uid uidAndData
            late = maybe False (problemSubmissionTime val `laterThan`) due
        (updateSubmissionWidget,enctypeUpdateSubmission) <- generateFormPost (identifyForm "updateSubmission" $ updateSubmissionForm extra ident (show uid))
        let isGraded = if correct then "graded"
                                  else case extra of Just _ -> "graded"; _ -> "ungraded" :: String
            howGraded = if correct then "automatically graded"
                                   else case extra of Just _ -> "manually graded"; _ -> "ungraded" :: String
            credit =  case problemSubmissionCredit val of Just n -> n; _ -> 5
            latescore = maybe (floor ((fromIntegral credit :: Rational) / 2)) id (problemSubmissionLateCredit val)
            lateIndicator = if late then "(late)" else "" :: String
            score | correct && not late = credit
                  | correct = latescore
                  | otherwise = 0
            awarded = maybe "0" show extra
            mailto email = sanitizeHtml email ++ "?subject=[Carnap-" ++ sanitizeHtml ident ++ "]"
            template display =
                [whamlet|
                    <div.card.mb-3.#{isGraded} data-submission-uid="#{show uid}">
                        <div.card-body style="padding:20px">
                            <div.d-flex.flex-wrap-reverse>
                                <div.col-sm-8>
                                    <h4.card-title>#{ident}
                                    <div style="overflow-x:scroll">
                                        ^{display}
                                <div.col-sm-4>
                                    <h6.review-status>#{howGraded}
                                    <h6.point-value>point value: #{credit}
                                    <h6.points-score>submission score: #{score} #{lateIndicator}
                                    <h6.points-awarded>points added: #{awarded}
                                    <hr>
                                    <form.updateSubmission enctype=#{enctypeUpdateSubmission}>
                                        ^{updateSubmissionWidget}
                                        <div.form-group>
                                            <input.btn.btn-primary type=submit value="update" disabled>
                                    <hr>
                                    $maybe userdata' <- entityVal <$> userdata
                                        $maybe email <- userDataEmail userdata'
                                            <a href="mailto:#{mailto email}">
                                                <i.fa.fa-envelope-o>
                                                email student
                                        $nothing
                                            <span>
                                                <i.fa.fa-envelope-o>
                                                email address unavailable
                                        <br>
                                        <a href="#" onclick="tryDeleteProblem(event,'#{jsonSerialize $ DeleteProblem key}')">
                                            <i.fa.fa-trash-o>
                                            delete problem
                |]
        case (problemSubmissionType val, problemSubmissionData val) of
            (Derivation, DerivationData goal der) -> template
                [whamlet|
                    <div data-carnap-system="prop"
                         data-carnap-options="resize"
                         data-carnap-type="proofchecker"
                         data-carnap-goal="#{goal}"
                         data-carnap-submission="none">
                         #{der}
                |]
            (Derivation, DerivationDataOpts goal der opts) -> template
                [whamlet|
                    <div data-carnap-type="proofchecker"
                         data-carnap-system="#{sys}"
                         data-carnap-options="#{reviewOptions}"
                         data-carnap-guides="#{guides}"
                         data-carnap-tests="#{tests}"
                         data-carnap-goal="#{formatContent (unpack goal)}"
                         data-carnap-submission="none">
                         #{der}
                |]
                where tests = maybe "" id $ lookup "tests" (M.fromList opts)
                      reviewOptions :: String
                      reviewOptions = "resize" ++ if "render" `inOpts` M.fromList opts then " render" else ""
                                               ++ if "guides" `inOpts` M.fromList opts then " guides" else ""
                      sys = maybe "prop" id $ lookup "system" (M.fromList opts)
                      guides = maybe "none" id $ lookup "guides" (M.fromList opts)
                      formatContent c = maybe c id $ (ndNotation `ofPropSys` sys)
                                             `mplus` (ndNotation `ofFOLSys` sys)
                                             `mplus` (ndNotation `ofSecondOrderSys` sys)
                                             `mplus` (ndNotation `ofSetTheorySys` sys)
                                             `mplus` (ndNotation `ofDefiniteDescSys` sys)
                                             `mplus` (ndNotation `ofModalPropSys` sys) <*> Just c
            (TruthTable, TruthTableData goal tt) -> template
                [whamlet|
                    <div data-carnap-type="truthtable"
                         data-carnap-tabletype="#{checkvalidity goal}"
                         data-carnap-submission="none"
                         data-carnap-goal="#{goal}">
                         #{renderTT tt}
                |]
            (TruthTable, TruthTableDataOpts goal tt opts) -> template
                [whamlet|
                    <div data-carnap-type="truthtable"
                         data-carnap-tabletype="#{tabletype}"
                         data-carnap-system="#{ttsystem}"
                         data-carnap-truemark="#{trueMark}"
                         data-carnap-falsemark="#{falseMark}"
                         data-carnap-submission="none"
                         data-carnap-value=#{renderTT tt}
                         data-carnap-options="#{reviewOptions}"
                         data-carnap-goal="#{formatContent (unpack goal)}">
                         #{renderTT tt}
                |]
                where tabletype = case lookup "tabletype" (M.fromList opts) of Just s -> s; Nothing -> checkvalidity goal
                      ttsystem = case lookup "system" (M.fromList opts) of Just s -> s; Nothing -> "prop"
                      trueMark = case lookup "truemark" (M.fromList opts) of Just s -> s; Nothing -> "T"
                      falseMark = case lookup "falsemark" (M.fromList opts) of Just s -> s; Nothing -> "F"
                      reviewOptions :: String
                      reviewOptions = "immutable nocheck nocounterexample" ++ if "turnstilemark" `inOpts` M.fromList opts then " turnstilemark" else ""
                      formatContent c = case (ndNotation `ofPropSys` ttsystem) <*> maybeString of Just s -> s; Nothing -> ""
                        where maybeString = (show <$> (readMaybe c :: Maybe PureForm))
                                    `mplus` (intercalate "," . map show <$> (readMaybe c :: Maybe [PureForm]))
                                    `mplus` case readMaybe c :: Maybe ([PureForm],[PureForm]) of
                                                      Just (fs,gs) -> Just $ intercalate "," (map show fs) ++ ":" ++ intercalate "," (map show gs)
                                                      Nothing -> Just c --If it's a sequent, it'll show properly anyway.

            (CounterModel, CounterModelDataOpts goal cm opts) -> template
                [whamlet|
                    <div data-carnap-type="countermodeler"
                         data-carnap-countermodelertype="#{cmtype}"
                         data-carnap-submission="none"
                         data-carnap-system="#{cmsystem}"
                         data-carnap-goal="#{formatContent (unpack goal)}">
                         #{renderCM cm}
                |]
                where cmtype = case lookup "countermodelertype" (M.fromList opts) of Just s -> s; Nothing -> checkvalidity goal
                      cmsystem = case lookup "system" (M.fromList opts) of Just s -> s; Nothing -> "firstOrder"
                      formatContent c = case (ndNotation `ofFOLSys` cmsystem) <*> maybeString of Just s ->  s; Nothing -> ""
                        where maybeString = (show <$> (readMaybe c :: Maybe PureForm))
                                    `mplus` (intercalate "," . map show <$> (readMaybe c :: Maybe [PureFOLForm]))
                                    `mplus` Just c --If it's a sequent, it'll show properly anyway.

            (Translation, TranslationData goal trans) -> template
                [whamlet|
                    <div data-carnap-type="translate"
                         data-carnap-transtype="prop"
                         data-carnap-goal="#{show (simpleCipher (unpack goal))}"
                         data-carnap-submission="none"
                         data-carnap-problem="#{goal}">
                         #{trans}
                |]
            (Translation, TranslationDataOpts goal trans opts) -> template
                [whamlet|
                    <div data-carnap-type="translate"
                         data-carnap-transtype="#{transtype}"
                         data-carnap-system="#{sys}"
                         data-carnap-goal="#{show (simpleCipher (formatContent (unpack goal)))}"
                         data-carnap-submission="none"
                         data-carnap-problem="#{problem}">
                         #{trans}
                |]
                where transtype = case lookup "transtype" (M.fromList opts) of Just s -> s; Nothing -> "prop"
                      formatContent c = case transtype of
                                            "prop" -> maybe c id $ ndNotation `ofPropSys` sys <*> Just c
                                            "first-order" -> maybe c id $ ndNotation `ofFOLSys` sys <*> Just c
                                            "description" -> maybe c id $ ndNotation `ofDefiniteDescSys` sys <*> Just c
                      problem = case lookup "problem" (M.fromList opts) of Just s -> s ++ " : " ++ (formatContent $ unpack goal)
                      sys = case lookup "system" (M.fromList opts) of
                                Just s -> s;
                                Nothing -> if transtype == "prop" then "prop" else "firstOrder"
            (SequentCalc, SequentCalcData goal tree opts) -> template
                [whamlet|
                    <div data-carnap-type="sequentchecker"
                         data-carnap-system="#{sys}"
                         data-carnap-options="displayJSON"
                         data-carnap-goal="#{goal}"
                         data-carnap-submission="none">
                         #{treeJSON tree}
                |]
                where sys = case lookup "system" (M.fromList opts) of Just s -> s; Nothing -> "propLK"
            (DeductionTree, DeductionTreeData goal tree opts) -> template
                [whamlet|
                    <div data-carnap-type="treedeductionchecker"
                         data-carnap-system="#{sys}"
                         data-carnap-options="displayJSON"
                         data-carnap-goal="#{goal}"
                         data-carnap-submission="none">
                         #{treeJSON tree}
                |]
                where sys = case lookup "system" (M.fromList opts) of Just s -> s; Nothing -> "propNK"
            (Qualitative, QualitativeProblemDataOpts goal answer opts) -> template
                [whamlet|
                    <div data-carnap-type="qualitative"
                         data-carnap-qualitativetype="#{qualtype}"
                         data-carnap-goal="#{unpack goal}"
                         data-carnap-submission="none">
                         #{content}
                |]
                where qualtype = case lookup "qualitativetype" (M.fromList opts) of Just s -> s; Nothing -> "shortanswer"
                      content = case qualtype of
                                    "shortanswer"    -> unpack answer
                                    "multiplechoice" -> withChecked
                      withChecked = case lookup "content" (M.fromList opts) of
                                        Nothing -> "Error no content"
                                        Just cnt -> unlines . map processEntry . lines . unpack $ cnt
                      processEntry l = case readMaybe l :: Maybe (Int, String) of
                                           Just (h,s) | s == unpack answer && h == simpleHash ('+':s) -> show (simpleHash ('+':s), s)
                                           Just (h,s) | s == unpack answer && h == simpleHash ('*':s) -> show (simpleHash ('+':s), s)
                                           Just (_h,s) | s == unpack answer -> show (simpleHash ('-':s), s)
                                           Just (h,s) | h == simpleHash ('-':s) -> show (simpleHash s, s)
                                           Just (h,s) | h == simpleHash ('+':s) -> show (simpleHash ('*':s), s)
                                           Just (h,s) -> show (h, s)
                                           Nothing -> "indeciperable entry"
            (Qualitative, QualitativeMultipleSelection goal answer opts) -> template
                [whamlet|
                    <div data-carnap-type="qualitative"
                         data-carnap-qualitativetype="multipleselection"
                         data-carnap-goal="#{unpack goal}"
                         data-carnap-submission="none">
                         #{content}
                |]
                where content = case lookup "content" (M.fromList opts) of
                                        Nothing -> "Error no content"
                                        Just cnt -> unlines . map processEntry . lines . unpack $ cnt
                      tryMaybe (Just True) = True
                      tryMaybe _           = False
                      processEntry l = case readMaybe l :: Maybe (Int, String) of
                                           Just (h,s) | h == simpleHash ('*':s) && tryMaybe (lookup s answer) -> show (simpleHash ('+':s), s)
                                           Just (h,s) | h == simpleHash ('+':s) && not (tryMaybe (lookup s answer)) -> show (simpleHash ('*':s), s)
                                           Just (h,s) | h == simpleHash s && not (tryMaybe (lookup s answer)) -> show (simpleHash ('-':s), s)
                                           Just (h,s) | h == simpleHash ('-':s) && tryMaybe (lookup s answer) -> show (simpleHash s, s)
                                           Just (h,s) -> show (h, s)
                                           Nothing -> "indeciperable entry"

            (Qualitative, QualitativeNumericalData problem answer opts) -> template
                [whamlet|
                    <div data-carnap-type="qualitative"
                         data-carnap-qualitativetype="#{qualtype}"
                         data-carnap-problem="#{unpack problem}"
                         data-carnap-goal="#{goal}"
                         data-carnap-submission="none">
                         #{show answer}
                |]
                where qualtype = case lookup "qualitativetype" (M.fromList opts) of Just s -> s; Nothing -> "shortanswer"
                      goal = case lookup "goal" (M.fromList opts) of Just s -> s; Nothing -> "no goal?"

            (_, ProblemContent txt) -> template
                [whamlet|
                    <div>
                        <p>Submission: #{txt}
                        <p>Autograded, no data for review
                |]
            _ -> return ()
    where renderTT tt = concat $ map renderRow tt
          renderRow row = map toval row ++ "\n"
          toval (Just True)  = 'T'
          toval (Just False) = 'F'
          toval Nothing      = '-'
          checkvalidity ct = if '⊢' `elem` ct then "validity" :: String else "simple" :: String
          renderCMPair (l,v) = l ++ ":" ++ v
          renderCM cm = unlines . map renderCMPair $ cm
          escapeForJSON s = concatMap escape s
            where escape '\b' = "\\b"
                  escape '\n' = "\\n"
                  escape '\t' = "\\t"
                  escape '\r' = "\\r"
                  escape '\f' = "\\f"
                  escape '\\' = "\\\\"
                  escape '"'  = "\\\""
                  escape c    = [c]

          treeJSON (Node (l,r) f) = "{\"label\":\"" ++ escapeForJSON l
                                  ++ "\",\"rule\":\"" ++ escapeForJSON r
                                  ++ "\",\"forest\":["
                                  ++ intercalate "," (map treeJSON f)
                                  ++ "]}"

updateSubmissionForm
    :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
    => Maybe Int
    -> Text
    -> String
    -> Markup
    -> MForm m (FormResult (Text, String, Int), WidgetFor (HandlerSite m) ())
updateSubmissionForm extra ident uid = renderBootstrap3 BootstrapBasicForm $ (,,)
            <$> areq hiddenField "" (Just ident)
            <*> areq hiddenField "" (Just uid)
            <*> areq scoreField scoreInput {fsAttrs = ("autocomplete","off") : fsAttrs scoreInput}
                                (maybe (Just 0) Just extra)
    where scoreField = check validateScore intField
          validateScore s | s < 0 = Left ("Added credit must be a positive number." :: Text)
                          | otherwise = Right s
          scoreInput =  bfs ("Partial Credit Points"::Text)

data ReviewDelete = DeleteProblem ProblemSubmissionId
   deriving Generic

instance ToJSON ReviewDelete

instance FromJSON ReviewDelete

