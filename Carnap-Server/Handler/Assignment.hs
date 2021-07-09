module Handler.Assignment (postCourseAssignmentR, getCourseAssignmentR, getCourseAssignmentStateR, putCourseAssignmentStateR) where

import           Data.Aeson.Types
import           Data.Time
import           Data.Time.Clock.POSIX
import           Import
import           Text.Blaze.Html              (Markup, toMarkup)
import           Text.Julius                  (juliusFile, rawJS)
import           Text.Pandoc                  (Block, lookupMeta)
import           TH.RelativePaths             (pathRelativeToCabalPackage)
import           Yesod.Form.Bootstrap3

import           Filter.Randomize
import           Util.Assignment
import           Util.Data
import           Util.Database
import           Util.Handler

getCourseAssignmentR :: Text -> Text -> Handler Html
getCourseAssignmentR coursetitle filename = getAssignmentAndPathByCourse coursetitle filename
                                            >>= uncurry (returnAssignment coursetitle filename)

putCourseAssignmentStateR :: Text -> Text -> Handler Value
putCourseAssignmentStateR _coursetitle _filename = do
        msg <- requireCheckJsonBody :: Handler Value
        uid <- maybeAuthId >>= maybe reject return
        let maid = parseMaybe (withObject "assignment key" (.: "assignmentKey")) msg :: Maybe Text
        aid <- maybe (sendStatusJSON badRequest400 ("Ill-formed assignment key" :: Text)) return (maid >>= readMay :: Maybe (Key AssignmentMetadata))
        runDB $ upsert (AssignmentState msg uid aid) [AssignmentStateValue =. msg]
        returnJson msg

getCourseAssignmentStateR :: Text -> Text -> Handler Value
getCourseAssignmentStateR _coursetitle _filename = do
        uid <- maybeAuthId >>= maybe reject return
        maid <- lookupGetParam "aid"
        aid <- maybe (sendStatusJSON badRequest400 ("Ill-formed assignment key" :: Text)) return (maid >>= readMay :: Maybe (Key AssignmentMetadata))
        mstate <- runDB $ getBy (UniqueAssignmentState uid aid)
        case mstate of
            Just (Entity _ state) -> returnJson (assignmentStateValue state)
            Nothing               -> returnJson (mempty :: Object)

postCourseAssignmentR :: Text -> Text -> Handler Html
postCourseAssignmentR coursetitle filename = do
        Entity key val <- getAssignmentByCourse coursetitle filename
        uid <- maybeAuthId >>= maybe reject return
        ((passrslt,_),_) <- runFormPost (identifyForm "enterPassword" $ enterPasswordForm)
        case passrslt of
            FormSuccess password ->
                let insertToken = do currentTime <- liftIO getCurrentTime
                                     mtoken <- runDB $ insertUnique $ AssignmentAccessToken currentTime key uid
                                     case mtoken of
                                            Nothing -> $logWarn "couldn't insert access token. Double POST?"
                                            _ -> return ()
                                     setMessage $ "Access Granted"
                in case assignmentMetadataAvailability val of
                        Just restrict | password == availabilityPassword restrict -> insertToken
                        _ -> setMessage $ "Incorrect Access Key"
            FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
            FormMissing -> setMessage $ "Form Missing"
        redirect $ CourseAssignmentR coursetitle filename

returnAssignment :: Text -> Text -> Entity AssignmentMetadata -> FilePath -> Handler Html
returnAssignment coursetitle filename (Entity key val) path = do
        uid <- maybeAuthId >>= maybe reject return
        Entity _ userdata <- maybeUserData >>= maybe reject return
        (maccess,mtoken,mextension) <- runDB $ do
               maccess <- getBy $ UniqueAccommodation (assignmentMetadataCourse val) uid
               mtoken <- getBy $ UniqueAssignmentAccessToken uid key
               mextension <- getBy $ UniqueExtension key uid
               return (maccess,mtoken,mextension)
        time <- liftIO getCurrentTime
        let accommodationFactor = maybe 1 (accommodationTimeFactor . entityVal) maccess
            accommodationMinutes = maybe 0 (accommodationTimeExtraMinutes . entityVal) maccess
            testTime :: Int -> Int
            testTime min = floor ((fromIntegral min) * accommodationFactor) + accommodationMinutes
            instructorAccess = userDataInstructorId userdata /= Nothing --instructors who shouldn't access the course are already blocked by yesod-auth
            age (Entity _ tok) = floor (diffUTCTime time (assignmentAccessTokenCreatedAt tok))

            creation :: Entity AssignmentAccessToken -> Integer
            creation (Entity _ tok) = round $ utcTimeToPOSIXSeconds (assignmentAccessTokenCreatedAt tok) * 1000 --milliseconds to match JS

            jsMaybe f v = maybe (rawJS ("null" :: Text)) (rawJS . f) v
            toJsTime = show . toMS . round . utcTimeToPOSIXSeconds
            toMS :: Integer -> Integer
            toMS x = x * 1000
        if visibleAt time val mextension || instructorAccess
            then do
                ehtml <- liftIO $ fileToHtml (assignmentFilters (hash (show uid ++ path))) path
                unless (visibleAt time val mextension) $ setMessage "Viewing as instructor. Assignment not currently visible to students."
                case ehtml of
                    Left err -> defaultLayout $ minimalLayout (show err)
                    Right (Left err,_) -> defaultLayout $ minimalLayout (show err)
                    Right (Right html,meta) -> case (assignmentMetadataAvailability val, mtoken) of
                        (Just _, Nothing) -> defaultLayout $ do
                             (enterPasswordWidget,enctypeEnterPassword) <- generateFormPost (identifyForm "enterPassword" $ enterPasswordForm)
                             $(widgetFile "passwordEntry")
                        (Just (ViaPasswordExpiring _ min), Just tok) | age tok > 60 * testTime min && not instructorAccess ->
                             defaultLayout $ minimalLayout ("Assignment time limit exceeded" :: String)
                        (Just (HiddenViaPasswordExpiring _ min), Just tok) | age tok > 60 * testTime min && not instructorAccess ->
                             defaultLayout $ minimalLayout ("Assignment time limit exceeded" :: String)
                        (mavail,_) -> do
                             mbcss <- retrievePandocVal (lookupMeta "base-css" meta)
                             mcss <- retrievePandocVal (lookupMeta "css" meta)
                             mjs <- retrievePandocVal (lookupMeta "js" meta)
                             let source = "assignment:" ++ show key
                                 theLayout = \widget -> case mbcss of
                                    Nothing -> defaultLayout $ do mapM addStylesheet [StaticR css_bootstrapextra_css]
                                                                  widget
                                    Just bcss -> cleanLayout $ do mapM addStylesheetRemote bcss
                                                                  widget
                             theLayout $ do
                                 toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/command.julius")
                                 toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/status-warning.julius")
                                 toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/assignment-state.julius")
                                 toWidgetHead [julius|var submission_source="#{rawJS source}";|]
                                 toWidgetHead [julius|var assignment_key="#{rawJS $ show key}";|]
                                 case (mavail >>= availabilityMinutes,mtoken) of
                                     (Just min, Just tok) -> toWidgetHead [julius|
                                                                             var availability_minutes = #{rawJS $ show (testTime min)};
                                                                             var token_time = #{rawJS $ show $ creation tok};
                                                                           |]
                                     (_,_) -> return ()

                                 addDocScripts

                                 maybe (pure [()]) (mapM addStylesheetRemote) mcss
                                 $(widgetFile "document")
                                 toWidgetBody [julius|CarnapServerAPI.getAssignmentState();|]
                                 maybe (pure [()]) (mapM addScriptRemote) mjs >> return ()
            else defaultLayout $ minimalLayout ("Assignment not currently set as visible by instructor" :: Text)
    where visibleAt t a mex = not (tooEarly t a) && not (tooLate t a mex)
          tooEarly t a | null (assignmentMetadataVisibleFrom a) = False
                       | otherwise = Just t < assignmentMetadataVisibleFrom a
          tooLate _ a _ | null (assignmentMetadataVisibleTill a) = False
          tooLate t a Nothing = assignmentMetadataVisibleTill a < Just t
          tooLate t a (Just (Entity _ ex)) = (extensionUntil ex < t) && (assignmentMetadataVisibleTill a < Just t)

assignmentFilters :: Int -> Block -> Block
assignmentFilters salt = randomizeProblems salt . allFilters

enterPasswordForm
    :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
    => Markup
    -> MForm m (FormResult Text, WidgetFor (HandlerSite m) ())
enterPasswordForm = renderBootstrap3 BootstrapBasicForm $ id
            <$> areq textField (bfs ("Access Key" :: Text)) Nothing

reject :: (MonadHandler m, RedirectUrl (HandlerSite m) (Route App)) => m b
reject = setMessage "you need to be logged in and fully registered to access assignments" >> redirect HomeR
