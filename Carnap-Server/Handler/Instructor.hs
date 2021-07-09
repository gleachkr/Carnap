{-# LANGUAGE DeriveGeneric #-}
module Handler.Instructor where

import           Control.Monad         (fail)
import qualified Data.Aeson            as A
import qualified Data.IntMap           (delete, fromList, insert, toList)
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Time
import           Data.Time.Zones
import           Data.Time.Zones.All
import           Data.Time.Zones.DB
import           Import
import           System.Directory      (doesFileExist, removeFile)
import           System.FilePath       (takeExtension)
import           Text.Blaze.Html       (Markup, toMarkup)
import           Text.Read             (readMaybe)
import           Util.API
import           Util.Data
import           Util.Database
import           Util.Grades
import           Util.Handler
import           Util.LTI
import           Yesod.Form.Bootstrap3
import           Yesod.Form.Jquery

putInstructorR :: Text -> Handler Value
putInstructorR ident = do
        requireReferral (InstructorR ident)
        ((assignmentrslt,_),_) <- runFormPost (identifyForm "updateAssignment" $ updateAssignmentForm)
        ((courserslt,_),_) <- runFormPost (identifyForm "updateCourse" $ updateCourseForm Nothing Nothing [])
        ((documentrslt,_),_) <- runFormPost (identifyForm "updateDocument" $ updateDocumentForm)
        ((accommodationrslt,_),_) <- runFormPost (identifyForm "updateAccommodation" $ updateAccommodationForm)
        ((extensionrslt,_),_) <- runFormPost  (identifyForm "updateExtension" $ updateExtensionForm [])
        case (assignmentrslt,courserslt,documentrslt,accommodationrslt,extensionrslt) of
            (FormSuccess theUpdate,_,_,_,_) -> do
                 k <- maybe (sendStatusJSON badRequest400 ("Could not read assignment key" :: Text)) return $ readMaybe (instructorAssignUpdateIdString theUpdate)
                 val <- runDB (get k) >>= maybe (sendStatusJSON notFound404 ("Could not find assignment" :: Text)) pure
                 let cid = assignmentMetadataCourse val
                 checkCourseOwnership ident cid
                 runDB $ do course <- get cid >>= maybe (sendStatusJSON notFound404 ("Could not find course" :: Text)) pure
                            let (Just tz) = tzByName . courseTimeZone $ course
                            let maccess = case ( instructorAssignUpdatePassword theUpdate
                                               , instructorAssignUpdateHidden theUpdate
                                               , instructorAssignUpdateTimeLimit theUpdate
                                               ) of
                                  (Nothing,_,_) -> Nothing
                                  (Just txt, Just True, Nothing) -> Just (HiddenViaPassword txt)
                                  (Just txt, Just True, Just mins) -> Just (HiddenViaPasswordExpiring txt mins)
                                  (Just txt, _, Just mins) -> Just (ViaPasswordExpiring txt mins)
                                  (Just txt, _, _) -> Just (ViaPassword txt)
                            update k [ AssignmentMetadataDuedate =. localTimeToUTCTZ tz <$> instructorAssignUpdateDue theUpdate
                                     , AssignmentMetadataVisibleFrom =. localTimeToUTCTZ tz <$> instructorAssignUpdateFrom theUpdate
                                     , AssignmentMetadataVisibleTill =. localTimeToUTCTZ tz <$> instructorAssignUpdateTill theUpdate
                                     , AssignmentMetadataGradeRelease =. localTimeToUTCTZ tz <$> instructorAssignUpdateRelease theUpdate
                                     , AssignmentMetadataPointValue =. instructorAssignUpdatePointValue theUpdate
                                     , AssignmentMetadataTotalProblems =. instructorAssignUpdateProblemCount theUpdate
                                     , AssignmentMetadataAvailability =. maccess
                                     , AssignmentMetadataAvailability =. maccess
                                     , AssignmentMetadataDescription =. unTextarea <$> instructorAssignUpdateDescription theUpdate
                                     ]
                 returnJson ("updated!"::Text)
            (_,FormSuccess (UpdateCourse idstring mdesc mstart mend mpoints mopen mtext mLtiId),_,_,_) -> do
                 cid <- maybe (sendStatusJSON badRequest400 ("Could not read course key" :: Text))
                        return . readMaybe . T.unpack $ idstring
                 checkCourseOwnership ident cid
                 runDB $ do course <- get cid >>= maybe (sendStatusJSON badRequest400 ("could not find course" :: Text)) pure
                            let Just tz = tzByName . courseTimeZone $ course
                                unlocalize day = localTimeToUTCTZ tz (LocalTime day (TimeOfDay 23 59 59))
                            update cid [ CourseDescription =. (unTextarea <$> mdesc) ]
                            update cid [ CourseTextBook =. mtext]
                            maybeDo mstart (\start -> update cid
                              [ CourseStartDate =. unlocalize start])
                            maybeDo mend (\end-> update cid
                              [ CourseEndDate =. unlocalize end])
                            maybeDo mpoints (\points-> update cid
                              [ CourseTotalPoints =. points ])
                            maybeDo mopen (\open -> update cid
                              [ CourseEnrollmentOpen =. open])

                            let mnewLtiId = A.decode =<< (fromStrict . encodeUtf8 <$> mLtiId)
                                autoregKey = CourseAutoregKey cid
                            -- if the autoreg form field is
                            -- cleared/invalidated, we delete the course's auto
                            -- registration record. otherwise update/add it
                            maybe (delete autoregKey)
                                (\(AutoregTriple lab iss did k) ->
                                    repsert autoregKey $ CourseAutoreg lab iss did k cid)
                                mnewLtiId

                 returnJson ("updated!"::Text)
            (_,_,FormSuccess (idstring, mscope, mdesc,mfile,mtags),_,_) -> do
                 k <- maybe (sendStatusJSON badRequest400 ("Could not read document key" :: Text)) return $ readMaybe idstring
                 doc <- runDB (get k) >>= maybe (sendStatusJSON notFound404 ("Could not find document" :: Text)) pure
                 ident' <- getIdent (documentCreator doc) >>= maybe (sendStatusJSON notFound404 ("Could not find document creator" :: Text)) pure
                 if ident == ident' then return () else sendStatusJSON forbidden403 ("You don't seem to own this document" :: Text)
                 runDB $ do update k [ DocumentDescription =. (unTextarea <$> mdesc) ]
                            maybeDo mscope (\scope -> update k [ DocumentScope =. scope ])
                            maybeDo mtags (\tags -> do
                                              oldTags <- selectList [TagBearer ==. k] []
                                              mapM_ (delete . entityKey) oldTags
                                              forM_ tags (\tag -> insert_ $ Tag k tag)
                                              return ())
                 maybeDo mfile (saveTo ("documents" </> unpack ident) (unpack $ documentFilename doc))
                 returnJson ("updated!" :: Text)
            (_,_,_,FormSuccess (cidstring, uidstring, mextramin, mfactor,mextrahours),_) -> do
                 cid <- maybe (sendStatusJSON badRequest400 ("Could not read course key" :: Text)) return $ readMaybe cidstring
                 checkCourseOwnership ident cid
                 uid <- maybe (sendStatusJSON badRequest400 ("Could not read user key" :: Text)) return $ readMaybe uidstring
                 do runDB $ upsertBy (UniqueAccommodation cid uid)
                                     (Accommodation cid uid
                                        (maybe 1 id mfactor)
                                        (maybe 0 id mextramin)
                                        (maybe 0 id mextrahours))
                                     (maybe [] (\min -> [AccommodationTimeExtraMinutes =. min]) mextramin ++
                                      maybe [] (\fac -> [AccommodationTimeFactor =. fac]) mfactor ++
                                      maybe [] (\hours-> [AccommodationDateExtraHours =. hours]) mextrahours)
                 returnJson ("updated!" :: Text)
            (_,_,_,_,FormSuccess (uidstring, aid, day, mtime)) -> do
                 uid <- maybe (sendStatusJSON badRequest400 ("Couldn't read uid string" :: Text)) pure $ (jsonDeSerialize uidstring :: Maybe (Key User))
                 let localtime = LocalTime day (maybe (TimeOfDay 23 59 59) id mtime)
                 runDB $ do asgn <- get aid >>= maybe (sendStatusJSON notFound404 ("Couldn't get assignment" :: Text)) pure
                            course <- get (assignmentMetadataCourse asgn) >>= maybe (liftIO $ fail "could not get course assignment") pure
                            tz <- maybe (liftIO $ fail "couldn't read timezone") pure $ fromTZName $ courseTimeZone course
                            let utctime = localTimeToUTCTZ (tzByLabel tz) localtime
                            upsertBy (UniqueExtension aid uid)
                                     (Extension aid uid utctime)
                                     [ExtensionUntil =. utctime]
                 returnJson ("updated!" :: Text)
            (FormMissing,FormMissing,FormMissing,FormMissing,FormMissing) -> sendStatusJSON badRequest400 ("Form Missing" :: Text)
            (form1,form2,form3,form4,form5) -> sendStatusJSON badRequest400 ("errors: " <> errorsOf form1 <> errorsOf form2 <> errorsOf form3 <> errorsOf form4 <> errorsOf form5)
                where errorsOf (FormFailure s) = concat s <> ", "
                      errorsOf _               = ""

deleteInstructorR :: Text -> Handler Value
deleteInstructorR ident = do
    requireReferral (InstructorR ident)
    msg <- requireCheckJsonBody :: Handler InstructorDelete
    case msg of
      DeleteAssignment aid -> do
           asgn <- runDB (get aid) >>= maybe (sendStatusJSON notFound404 ("Couldn't get assignment" :: Text)) pure
           let cid = assignmentMetadataCourse asgn
           checkCourseOwnership ident cid
           runDB $ do
               course <- get cid >>= maybe (sendStatusJSON notFound404 ("Couldn't get course of assignment" :: Text)) pure
               if courseTextBook course == Just aid then update cid [CourseTextBook =. Nothing] else return ()
               deleteCascade aid
           returnJson ("Assignment deleted" :: Text)
      DeleteProblems coursename setnum -> do
           Entity cid theclass <- runDB (getBy $ UniqueCourse coursename) >>= maybe (sendStatusJSON notFound404 ("Couldn't get course" :: Text)) pure
           checkCourseOwnership ident cid
           runDB $ case readAssignmentTable <$> courseTextbookProblems theclass of
               Just assign -> update cid [CourseTextbookProblems =. (Just $ BookAssignmentTable $ Data.IntMap.delete setnum assign)]
               Nothing -> sendStatusJSON notFound404 ("Couldn't get assignment table" :: Text)
           returnJson ("Assignment deleted" :: Text)
      DeleteCourse coursename -> do
           Entity cid _ <- runDB (getBy $ UniqueCourse coursename) >>= maybe (sendStatusJSON notFound404 ("No course to delete, for some reason." :: Text)) pure
           checkCourseOwnership ident cid
           runDB $ do
               studentsOf <- selectList [UserDataEnrolledIn ==. Just cid] []
               mapM_ (\s -> update (entityKey s) [UserDataEnrolledIn =. Nothing]) studentsOf
               deleteCascade cid
           returnJson ("Class Deleted"::Text)
      DeleteDocument fn -> do
          datadir <- appDataRoot <$> (appSettings <$> getYesod)
          runDB $ do
               usr <- getBy (UniqueUser ident) >>= maybe (sendStatusJSON notFound404 ("Couldn't get user by ident" :: Text)) pure
               Entity k _ <- getBy (UniqueDocument fn (entityKey usr)) >>= maybe (sendStatusJSON notFound404 ("Couldn't get document by name" :: Text)) pure
               asgns <- selectList [AssignmentMetadataDocument ==. k] []
               forM asgns $ \(Entity aid asgn) -> do
                    let cid = assignmentMetadataCourse asgn
                    course <- get cid >>= maybe (sendStatusJSON notFound404 ("Couldn't get course of assignment associated with this document" :: Text)) pure
                    if courseTextBook course == Just aid then update cid [CourseTextBook =. Nothing] else return ()
               deleteCascade k
          liftIO $ do fe <- doesFileExist (datadir </> "documents" </> unpack ident </> unpack fn)
                      if fe then removeFile (datadir </> "documents" </> unpack ident </> unpack fn)
                            else return ()
          returnJson (fn ++ " deleted")
      DropStudent uid -> do
          Entity udid ud <- (runDB $ getBy $ UniqueUserData uid) >>= maybe (sendStatusJSON notFound404 ("Couldn't get student to drop" :: Text)) pure
          maybe (return ()) (checkCourseOwnership ident) $ userDataEnrolledIn ud
          name <- runDB $ do
              update udid [UserDataEnrolledIn =. Nothing]
              return (userDataFirstName ud <> " " <> userDataLastName ud)
          returnJson ("Dropped " <> name)
      DeleteToken tokid -> do
          cid <- runDB $ do
              tok <- get tokid >>= maybe (sendStatusJSON notFound404 ("Couldn't find token" :: Text)) pure
              asgn <- get (assignmentAccessTokenAssignment tok) >>= maybe (sendStatusJSON notFound404 ("Couldn't find token" :: Text)) pure
              return $ assignmentMetadataCourse asgn
          checkCourseOwnership ident cid
          runDB (delete tokid)
          returnJson ("Timer reset" :: Text)
      DeleteCoInstructor ciid -> do
          runDB $ do
              asgns <- selectList [AssignmentMetadataAssigner ==. Just ciid] []
              forM asgns $ \(Entity aid asgn) -> do
                   let cid = assignmentMetadataCourse asgn
                   course <- get cid >>= maybe (sendStatusJSON notFound404 ("Couldn't get course of assignment associated with this coinstructor" :: Text)) pure
                   if courseTextBook course == Just aid then update cid [CourseTextBook =. Nothing] else return ()
              deleteCascade ciid
          returnJson ("Deleted this coinstructor" :: Text)

postInstructorR :: Text -> Handler Html
postInstructorR ident = do
    --CSRF protections for forms means we don't need to insepct referer here.
    time <- liftIO getCurrentTime
    classes <- classesByInstructorIdent ident
    let activeClasses = filter (\c -> courseEndDate (entityVal c) > time) classes
    docs <- documentsByInstructorIdent ident
    instructors <- runDB $ selectList [UserDataInstructorId !=. Nothing] []
    ((assignmentrslt,_),_) <- runFormPost (identifyForm "uploadAssignment" $ uploadAssignmentForm activeClasses docs)
    ((documentrslt,_),_)   <- runFormPost (identifyForm "uploadDocument" $ uploadDocumentForm)
    ((newclassrslt,_),_)   <- runFormPost (identifyForm "createCourse" createCourseForm)
    ((frombookrslt,_),_)   <- runFormPost (identifyForm "setBookAssignment" $ setBookAssignmentForm activeClasses)
    ((instructorrslt,_),_) <- runFormPost (identifyForm "addCoinstructor" $ addCoInstructorForm instructors ("" :: String))
    ((newapikeyrslt,_),_)  <- runFormPost (identifyForm "createAPIKey" createAPIKeyForm)
    case assignmentrslt of
        FormSuccess postedAssignment ->
            do let Entity cid theclass = instructorAssignCourse postedAssignment
                   Entity docId theDoc = instructorAssignFile postedAssignment
               checkCourseOwnershipHTML ident cid
               Entity _ user <- requireAuth
               iid <- instructorIdByIdent (userIdent user)
                        >>= maybe (setMessage "failed to retrieve instructor" >> notFound) pure
               mciid <- if courseInstructor theclass == iid
                            then return Nothing
                            else runDB $ getBy (UniqueCoInstructor iid cid)
               let Just tz = tzByName . courseTimeZone $ theclass
                   info = unTextarea <$> instructorAssignDescription postedAssignment
                   theassigner = mciid
                   thename = documentFilename theDoc
               currentTime <- liftIO getCurrentTime

               case instructorAssignPassword postedAssignment of
                   Nothing | instructorAssignHidden postedAssignment == Just True || instructorAssignTimeLimit postedAssignment /= Nothing
                           -> setMessage "Hidden and time-limited assignments must be password protected"
                   _ -> do success <- runDB $ insertUnique $ AssignmentMetadata
                                                { assignmentMetadataDocument = docId
                                                , assignmentMetadataTitle = maybe thename id (instructorAssignTitle postedAssignment)
                                                , assignmentMetadataDescription = info
                                                , assignmentMetadataAssigner = entityKey <$> theassigner
                                                , assignmentMetadataDuedate = localTimeToUTCTZ tz <$> instructorAssignDue postedAssignment
                                                , assignmentMetadataVisibleFrom = localTimeToUTCTZ tz <$> instructorAssignFrom postedAssignment
                                                , assignmentMetadataVisibleTill = localTimeToUTCTZ tz <$> instructorAssignTill postedAssignment
                                                , assignmentMetadataGradeRelease = localTimeToUTCTZ tz <$> instructorAssignRelease postedAssignment
                                                , assignmentMetadataPointValue = instructorAssignPointValue postedAssignment
                                                , assignmentMetadataTotalProblems = instructorAssignProblemCount postedAssignment
                                                , assignmentMetadataDate = currentTime
                                                , assignmentMetadataCourse = cid
                                                , assignmentMetadataAvailability =
                                                    case ( instructorAssignPassword postedAssignment
                                                         , instructorAssignHidden postedAssignment
                                                         , instructorAssignTimeLimit postedAssignment) of
                                                            (Nothing,_,_) -> Nothing
                                                            (Just txt, Just True, Nothing) -> Just (HiddenViaPassword txt)
                                                            (Just txt, Just True, Just mins) -> Just (HiddenViaPasswordExpiring txt mins)
                                                            (Just txt, _, Just mins) -> Just (ViaPasswordExpiring txt mins)
                                                            (Just txt, _, _) -> Just (ViaPassword txt)
                                                }
                           case success of Just _ -> return ()
                                           Nothing -> setMessage "This file has already been assigned for this course"
        FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case documentrslt of
        FormSuccess (file, sharescope, docdesc, subtime, mtags) ->
            do musr <- runDB $ getBy $ UniqueUser ident
               let fn = fileName file
                   info = unTextarea <$> docdesc
                   Just uid = musr -- FIXME: catch Nothing here
               if isInvalidFilename fn then invalidArgs ["Invalid filename:" ++ fn] else return ()
               success <- runDB $ insertUnique $ Document
                                        { documentFilename = fn
                                        , documentDate = subtime
                                        , documentCreator = entityKey uid
                                        , documentDescription = info
                                        , documentScope = sharescope
                                        }
               case success of
                    Just k -> do safeSaveTo ("documents" </> unpack ident) (unpack fn) file
                                 runDB $ maybeDo mtags (\tags -> do
                                            forM_ tags (\tag -> insert_ $ Tag k tag)
                                            return ())
                    Nothing -> setMessage "You already have a shared document with this name."
        FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case newclassrslt of
        FormSuccess (title, coursedesc, startdate, enddate, tzlabel) -> do
            miid <- instructorIdByIdent ident
            case miid of
                Just iid ->
                    do let localize x = localTimeToUTCTZ (tzByLabel tzlabel) (LocalTime x midnight)
                       success <- runDB $ insertUnique $ Course
                                               { courseTitle = title
                                               , courseDescription = unTextarea <$> coursedesc
                                               , courseInstructor = iid
                                               , courseTextbookProblems = Nothing
                                               , courseStartDate = localize startdate
                                               , courseEndDate = localize enddate
                                               , courseTotalPoints = 0
                                               , courseTimeZone = toTZName tzlabel
                                               , courseTextBook = Nothing
                                               , courseEnrollmentOpen = True
                                               }
                       case success of Just _ -> setMessage "Course Created"
                                       Nothing -> setMessage $ "Could not save. Course titles must be unique."
                                                            ++ "Consider adding your instutition or the current semester as a suffix."
                Nothing -> setMessage "you're not an instructor!"
        FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case frombookrslt of
        FormSuccess (Entity cid theclass, theassignment, duedate, mduetime) -> do
            checkCourseOwnershipHTML ident cid
            runDB $ do
                let Just tz = tzByName . courseTimeZone $ theclass
                    localdue = maybe (LocalTime duedate $ TimeOfDay 23 59 59) (LocalTime duedate) mduetime
                    due = localTimeToUTCTZ tz localdue
                case readAssignmentTable <$> courseTextbookProblems theclass of
                    Just assign -> update cid [CourseTextbookProblems =. (Just $ BookAssignmentTable $ Data.IntMap.insert theassignment due assign)]
                    Nothing -> update cid [CourseTextbookProblems =. (Just $ BookAssignmentTable $ Data.IntMap.fromList [(theassignment, due)])]
        FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case instructorrslt of
        (FormSuccess (cidstring , Just iid)) ->
            case readMaybe cidstring of
                Just cid -> do checkCourseOwnershipHTML ident cid
                               success <- runDB $ insertUnique $ CoInstructor iid cid
                               case success of Just _ -> setMessage "Added Co-Instructor!"
                                               Nothing -> setMessage "Co-Instructor seems to already be added"
                Nothing -> setMessage "Couldn't read cid string"
        FormSuccess (_, Nothing) -> setMessage "iid missing"
        FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case newapikeyrslt of
        FormSuccess () -> do Entity uid _ <- runDB (getBy $ UniqueUser ident) >>= maybe (permissionDenied "Couldn't find user data") return
                             Entity _ ud <- runDB (getBy $ UniqueUserData uid) >>= maybe (permissionDenied "Couldn't find user data") return
                             let UserData { userDataUserId = uid, userDataInstructorId = miid } = ud
                             iid <- maybe (permissionDenied "Only instructors can get API keys") return miid
                             newKey <- generateAPIKey
                             success <- runDB $ do deleteBy (UniqueAuthAPIUser uid) --clear existing key
                                                   insertUnique $ AuthAPI { authAPIKey = newKey
                                                                , authAPIUser = uid
                                                                , authAPIInstructorId = iid
                                                                , authAPIScope = APIKeyScopeRoot
                                                                }
                             case success of
                                 Just _ -> setMessage "API Key Created"
                                 _ -> setMessage "Couldn't delete old API Key"
        FormFailure s -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    redirect $ InstructorR ident

postInstructorQueryR :: Text -> Handler Value
postInstructorQueryR ident = do
    requireReferral (InstructorR ident)
    msg <- requireCheckJsonBody :: Handler InstructorQuery
    case msg of
        QueryAPIKey uid -> do
            mapiAuth <- runDB $ getBy (UniqueAuthAPIUser uid)
            case mapiAuth of
                Nothing -> returnJson ("None Issued." :: Text)
                Just (Entity _ (AuthAPI {authAPIKey = theKey})) -> returnJson (decodeUtf8 theKey)
        QueryGrade uid cid -> do
            checkCourseOwnership ident cid
            score <- scoreByIdAndClassTotal cid uid
            returnJson score
        QueryScores uid cid -> do
            checkCourseOwnership ident cid
            score <- scoreByIdAndClassPerProblem cid uid
            returnJson score
        QueryAccommodation uid cid -> do
            checkCourseOwnership ident cid
            maccommodation <- runDB $ getBy $ UniqueAccommodation cid uid
            case maccommodation of
                Nothing -> returnJson (0 :: Int, 1 :: Double, 0 :: Int)
                Just (Entity _ acc) -> returnJson ( accommodationTimeExtraMinutes acc
                                                  , accommodationTimeFactor acc
                                                  , accommodationDateExtraHours acc
                                                  )
        QueryTokens uid cid -> do
            checkCourseOwnership ident cid
            (toks, course) <- runDB $ do
                toks <- selectList [AssignmentAccessTokenUser ==. uid] []
                course <- get cid >>= maybe (sendStatusJSON notFound404 ("Could not find associated course" :: Text)) pure
                return (toks, course)
            let deletions = map (\(Entity k tok) ->
                                    ( assignmentAccessTokenAssignment tok
                                    , dateDisplay (assignmentAccessTokenCreatedAt tok) course
                                    , DeleteToken k)) toks
            returnJson deletions

getInstructorR :: Text -> Handler Html
getInstructorR ident = do
    musr <- runDB $ getBy $ UniqueUser ident
    case musr of
        Nothing -> defaultLayout nopage
        (Just (Entity uid _))  -> do
            UserData {userDataFirstName = firstname, userDataLastName = lastname} <- checkUserData uid
            classes <- classesByInstructorIdent ident
            time <- liftIO getCurrentTime
            let activeClasses = filter (\c -> courseEndDate (entityVal c) > time) classes
            let inactiveClasses = filter (\c -> courseEndDate (entityVal c) < time) classes

            autoregRecords <- runDB $ getMany (map (CourseAutoregKey . entityKey) activeClasses)
            instructors <- runDB $ selectList [UserDataInstructorId !=. Nothing] []
            let labels = map labelOf $ take (length activeClasses) [1::Int ..]

            let autoregForCourse = \course -> (M.lookup (CourseAutoregKey . entityKey $ course) autoregRecords)
            classWidgets <-
                mapM (\c -> classWidget instructors c (autoregForCourse c))
                     activeClasses

            assignmentMetadata <- concat <$> mapM listAssignmentMetadata activeClasses --Get the metadata
            documents <- runDB $ selectList [DocumentCreator ==. uid] []
            problemSetLookup <- mapM (\c -> (,)
                                    <$> pure (entityKey c)
                                    <*> (maybe mempty readAssignmentTable
                                        <$> (getProblemSets . entityKey $ c))
                                ) classes
            let assignmentLookup = map (\(Entity k v,_) ->
                                                ( k
                                                , assignmentMetadataTitle v
                                                , assignmentMetadataDate v
                                                , assignmentMetadataCourse v
                                                )) assignmentMetadata
            tagMap <- forM documents $ \doc -> do
                                     tags <- runDB $ selectList [TagBearer ==. entityKey doc] []
                                     return (entityKey doc, map (tagName . entityVal) tags)
            let tagsOf d = lookup d tagMap
                tagString d = case lookup d tagMap of Just tags -> intercalate "," tags; _ -> ""
            (createAssignmentWidget,enctypeCreateAssignment) <- generateFormPost (identifyForm "uploadAssignment" $ uploadAssignmentForm activeClasses documents)
            (uploadDocumentWidget,enctypeShareDocument) <- generateFormPost (identifyForm "uploadDocument" $ uploadDocumentForm)
            (setBookAssignmentWidget,enctypeSetBookAssignment) <- generateFormPost (identifyForm "setBookAssignment" $ setBookAssignmentForm activeClasses)
            (updateAssignmentWidget,enctypeUpdateAssignment) <- generateFormPost (identifyForm "updateAssignment" $ updateAssignmentForm)
            (updateAccommodationWidget,enctypeUpdateAccommodation) <- generateFormPost (identifyForm "updateAccommodation" $ updateAccommodationForm)
            (updateDocumentWidget,enctypeUpdateDocument) <- generateFormPost (identifyForm "updateDocument" $ updateDocumentForm)
            (updateCourseWidget,enctypeUpdateCourse) <- generateFormPost (identifyForm "updateCourse" (updateCourseForm Nothing Nothing []))
            (createCourseWidget,enctypeCreateCourse) <- generateFormPost (identifyForm "createCourse" createCourseForm)
            (createAPIKeyWidget,enctypeCreateAPIKey) <- generateFormPost (identifyForm "createAPIKey" createAPIKeyForm)
            defaultLayout $ do
                 addScript $ StaticR js_popper_min_js
                 addScript $ StaticR js_tagsinput_js
                 addScript $ StaticR js_bootstrap_min_js
                 addStylesheet $ StaticR css_tagsinput_css
                 setTitle $ "Instructor Page for " ++ toMarkup firstname ++ " " ++ toMarkup lastname
                 $(widgetFile "instructor")
    where labelOf = T.append "course-" . pack . show
          mprobsOf course = readAssignmentTable <$> courseTextbookProblems course
          nopage = [whamlet|
                    <div.container>
                        <p> Instructor not found.
                   |]

---------------------
--  Message Types  --
---------------------

data InstructorDelete = DeleteAssignment AssignmentMetadataId
                      | DeleteProblems Text Int
                      | DeleteCourse Text
                      | DeleteDocument Text
                      | DropStudent UserId
                      | DeleteToken AssignmentAccessTokenId
                      | DeleteCoInstructor CoInstructorId
    deriving Generic

instance ToJSON InstructorDelete

instance FromJSON InstructorDelete

data InstructorQuery = QueryGrade UserId CourseId
                     | QueryScores UserId CourseId
                     | QueryAccommodation UserId CourseId
                     | QueryTokens UserId CourseId
                     | QueryAPIKey UserId
    deriving Generic

instance ToJSON InstructorQuery

instance FromJSON InstructorQuery

data InstructorAssign = InstructorAssign
                      { instructorAssignFile         :: Entity Document
                      , instructorAssignTitle        :: Maybe Text
                      , instructorAssignCourse       :: Entity Course
                      , instructorAssignDue          :: Maybe LocalTime
                      , instructorAssignFrom         :: Maybe LocalTime
                      , instructorAssignTill         :: Maybe LocalTime
                      , instructorAssignRelease      :: Maybe LocalTime
                      , instructorAssignPointValue   :: Maybe Int
                      , instructorAssignProblemCount :: Maybe Int
                      , instructorAssignDescription  :: Maybe Textarea
                      , instructorAssignPassword     :: Maybe Text
                      , instructorAssignHidden       :: Maybe Bool
                      , instructorAssignTimeLimit    :: Maybe Int
                      }

data InstructorAssignUpdate = InstructorAssignUpdate
                      { instructorAssignUpdateIdString     :: String
                      , instructorAssignUpdateDue          :: Maybe LocalTime
                      , instructorAssignUpdateFrom         :: Maybe LocalTime
                      , instructorAssignUpdateTill         :: Maybe LocalTime
                      , instructorAssignUpdateRelease      :: Maybe LocalTime
                      , instructorAssignUpdatePointValue   :: Maybe Int
                      , instructorAssignUpdateProblemCount :: Maybe Int
                      , instructorAssignUpdateDescription  :: Maybe Textarea
                      , instructorAssignUpdatePassword     :: Maybe Text
                      , instructorAssignUpdateHidden       :: Maybe Bool
                      , instructorAssignUpdateTimeLimit    :: Maybe Int
                      }

------------------
--  Components  --
------------------

genericModal :: Text -> Text -> WidgetFor App () -> Enctype -> WidgetFor App ()
genericModal specific caption form enc =
    [whamlet|
        <div class="modal fade" id="update#{specific}Data" tabindex="-1" role="dialog" aria-labelledby="update#{specific}DataLabel" aria-hidden="true">
            <div class="modal-dialog" role="document">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="update#{specific}DataLabel">#{caption}
                        <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                          <span aria-hidden="true">&times;</span>
                    <div class="modal-body">
                        <form id="update#{specific}" enctype=#{enc}>
                            ^{form}
                            <div.form-group>
                                <input.btn.btn-primary type=submit value="update">
    |]

uploadAssignmentForm
    :: [Entity Course]
    -> [Entity Document]
    -> Markup
    -> MForm (HandlerFor App) (FormResult InstructorAssign, WidgetFor App ())
uploadAssignmentForm classes docs extra = do
            (fileRes, fileView) <- mreq (selectFieldList docnames) (bfs ("Document" :: Text)) Nothing
            (titleRes,titleView) <- mopt textField (withPlaceholder "Title (Defaults to Filename)" $ bfs ("Title "::Text)) Nothing
            (classRes, classView) <- mreq (selectFieldList classnames) (bfs ("Class" :: Text)) Nothing
            (dueRes,dueView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Due Date"::Text)) Nothing
            (duetimeRes, duetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
            (fromRes,fromView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible From Date"::Text)) Nothing
            (fromtimeRes, fromtimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible From Time"::Text)) Nothing
            (tillRes, tillView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible Until Date"::Text)) Nothing
            (tilltimeRes,tilltimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible Until Time"::Text)) Nothing
            (releaseRes,releaseView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Release Grades After Date"::Text)) Nothing
            (releasetimeRes,releasetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Release Grades After Time"::Text)) Nothing
            (pointValueRes,pointValueView) <- mopt intField (withPlaceholder "Points" $ bfs ("Point Value of Assignment"::Text)) Nothing
            (problemCountRes,problemCountView) <- mopt intField (withPlaceholder "Number of Problems" $ bfs ("Number of Problems for Assignment"::Text)) Nothing
            (descRes,descView) <- mopt textareaField (bfs ("Assignment Description"::Text)) Nothing
            (passRes,passView) <- mopt textField (bfs ("Password"::Text)) Nothing
            (hiddRes,hiddView) <- mopt checkBoxField (bfs ("Hidden"::Text)) Nothing
            (limitRes,limitView) <- mopt intField (bfs ("Limit"::Text)) Nothing
            let packTime Nothing _ = Nothing
                packTime (Just d) Nothing = Just $ LocalTime d (TimeOfDay 23 59 59)
                packTime (Just d) (Just t) = Just $ LocalTime d t
            let theRes = InstructorAssign
                                 <$> fileRes                                        -- instructorAssignFile :: Entity Document
                                 <*> titleRes                                       -- instructorAssignTitle :: Maybe Text
                                 <*> classRes                                       -- instructorAssignCourse :: Entity Course
                                 <*> (packTime <$> dueRes <*> duetimeRes)           -- instructorAssignDue :: Maybe LocalTime
                                 <*> (packTime <$> fromRes <*> fromtimeRes)         -- instructorAssignFrom :: Maybe LocalTime
                                 <*> (packTime <$> tillRes <*> tilltimeRes)         -- instructorAssignTill :: Maybe LocalTime
                                 <*> (packTime <$> releaseRes <*> releasetimeRes)   -- instructorAssignRelease :: Maybe LocalTime
                                 <*> pointValueRes                                  -- instructorAssignPointValue :: Maybe Int
                                 <*> problemCountRes                                -- instructorAssignProblemCount :: Maybe Int
                                 <*> descRes                                        -- instructorAssignDescription :: Maybe Textarea
                                 <*> passRes                                        -- instructorAssignPassword :: Maybe Text
                                 <*> hiddRes                                        -- instructorAssignHidden :: Maybe Bool
                                 <*> limitRes                                       -- instructorAssignTimeLimit :: Maybe Int
            let widget = [whamlet|
                #{extra}
                <h6>File to Assign
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput fileView}
                <h6>Assignment Title
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput titleView}
                <h6>Assign to
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput classView}
                <h6> Due Date
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput dueView}
                    <div.form-group.col-md-6>
                        ^{fvInput duetimeView}
                <h6> Visible From
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput fromView}
                    <div.form-group.col-md-6>
                        ^{fvInput fromtimeView}
                <h6> Visible To
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput tillView}
                    <div.form-group.col-md-6>
                        ^{fvInput tilltimeView}
                <h6> Release Grades After
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput releaseView}
                    <div.form-group.col-md-6>
                        ^{fvInput releasetimeView}
                <div.row>
                    <div.form-group.col-md-6>
                        <h6> Point Value of Assignment
                        ^{fvInput pointValueView}
                    <div.form-group.col-md-6>
                        <h6> Number of Problems
                        ^{fvInput problemCountView}
                <h6> Description
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput descView}
                <h5> Access Control Settings
                <div.row>
                    <div.col-md-6>
                         <h6> Password
                    <div.col-md-2>
                        <h6> Hide
                    <div.col-md-4>
                        <h6> Minutes Available
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput passView}
                    <div.form-group.col-md-2>
                        <span style="position:relative;top:7px">
                            Hidden:
                        <div style="display:inline-block;width:20px;position:relative;top:10px">
                            ^{fvInput hiddView}
                    <div.form-group.col-md-4>
                        ^{fvInput limitView}
                <p style="color:gray"> Note: all access control options require that you set a password.
                |]
            return (theRes,widget)
    where classnames = map (\theclass -> (courseTitle . entityVal $ theclass, theclass)) classes
          assignableDocs = filter isAssignable docs
          isAssignable thedoc = let extension =  takeExtension . unpack . documentFilename . entityVal $ thedoc
                                    in not (extension `elem` [".css",".js",".yaml",".png",".jpg",".jpeg",".gif",".svg",".pdf"])
          docnames = map (\thedoc -> (documentFilename . entityVal $ thedoc, thedoc)) assignableDocs

updateAssignmentForm
    :: Markup
    -> MForm (HandlerFor App) ((FormResult InstructorAssignUpdate, WidgetFor App ()))
updateAssignmentForm extra = do
            (assignmentRes,assignmentView) <- mreq assignmentId "" Nothing
            (dueRes,dueView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Due Date"::Text)) Nothing
            (duetimeRes, duetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
            (fromRes,fromView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible From Date"::Text)) Nothing
            (fromtimeRes, fromtimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible From Time"::Text)) Nothing
            (tillRes, tillView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible Until Date"::Text)) Nothing
            (tilltimeRes,tilltimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible Until Time"::Text)) Nothing
            (releaseRes,releaseView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Release Grades After Date"::Text)) Nothing
            (releasetimeRes,releasetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Release Grades After Time"::Text)) Nothing
            (pointValueRes,pointValueView) <- mopt intField (withPlaceholder "Points" $ bfs ("Point Value of Assignment"::Text)) Nothing
            (problemCountRes,problemCountView) <- mopt intField (withPlaceholder "Number of Problems" $ bfs ("Number of Problems for Assignment"::Text)) Nothing
            (descRes,descView) <- mopt textareaField (bfs ("Assignment Description"::Text)) Nothing
            (passRes,passView) <- mopt textField (bfs ("Password"::Text)) Nothing
            (hiddRes,hiddView) <- mopt checkBoxField (bfs ("Hidden"::Text)) Nothing
            (limitRes,limitView) <- mopt intField (bfs ("Limit"::Text)) Nothing
            let packTime Nothing _ = Nothing
                packTime (Just d) Nothing = Just $ LocalTime d (TimeOfDay 23 59 59)
                packTime (Just d) (Just t) = Just $ LocalTime d t
            let theRes = InstructorAssignUpdate
                             <$> assignmentRes                                  --instructorAssignUpdateIdString :: String
                             <*> (packTime <$> dueRes <*> duetimeRes)           --instructorAssignUpdateDue :: Maybe LocalTime
                             <*> (packTime <$> fromRes <*> fromtimeRes)         --instructorAssignUpdateFrom :: Maybe LocalTime
                             <*> (packTime <$> tillRes <*> tilltimeRes)         --instructorAssignUpdateTill :: Maybe LocalTime
                             <*> (packTime <$> releaseRes <*> releasetimeRes)   --instructorAssignUpdateRelease :: Maybe LocalTime
                             <*> pointValueRes                                  --instructorAssignUpdatePointValue :: Maybe Int
                             <*> problemCountRes                                --instructorAssignUpdateProblemCount :: Maybe Int
                             <*> descRes                                        --instructorAssignUpdateDescription :: Maybe Textarea
                             <*> passRes                                        --instructorAssignUpdatePassword :: Maybe Text
                             <*> hiddRes                                        --instructorAssignUpdateHidden :: Maybe Bool
                             <*> limitRes                                       --instructorAssignUpdateTimeLimit :: Maybe Int
            let widget = [whamlet|
                #{extra}
                ^{fvInput assignmentView}
                <h6> Due Date
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput dueView}
                    <div.form-group.col-md-6>
                        ^{fvInput duetimeView}
                <h6> Visible From
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput fromView}
                    <div.form-group.col-md-6>
                        ^{fvInput fromtimeView}
                <h6> Visible To
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput tillView}
                    <div.form-group.col-md-6>
                        ^{fvInput tilltimeView}
                <h6> Release Grades After
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput releaseView}
                    <div.form-group.col-md-6>
                        ^{fvInput releasetimeView}
                <div.row>
                    <div.form-group.col-md-6>
                       <h6> Point Value
                       ^{fvInput pointValueView}
                    <div.form-group.col-md-6>
                       <h6> Problem Count
                       ^{fvInput problemCountView}
                <h6> Description
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput descView}
                <h6> Access Control Settings
                <div.row>
                    <div.col-md-6>
                         <h6> Password
                    <div.col-md-2>
                        <h6> Hide
                    <div.col-md-4>
                        <h6> Minutes Available
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput passView}
                    <div.form-group.col-md-2>
                        <span style="position:relative;top:7px">
                        <div style="display:inline-block;width:20px;position:relative;top:10px">
                            ^{fvInput hiddView}
                    <div.form-group.col-md-4>
                        ^{fvInput limitView}
                <p style="color:gray"> Note: all access control options require that you set a password. Removing the password will remove all access control settings.
                |]
            return (theRes,widget)

    where assignmentId :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m String
          assignmentId = hiddenField

updateAssignmentModal :: WidgetFor App () -> Enctype -> WidgetFor App ()
updateAssignmentModal = genericModal "Assignment" "Update Assignment Data"

scopes :: [(Text, SharingScope)]
scopes = [ ("Everyone (Visible to everyone)", Public)
         , ("Instructors (Visible to all instructors)", InstructorsOnly)
         , ("Link Only (Available via the link, but unlisted)", LinkOnly)
         , ("Private (Not shared with other instructors, available to classes if assigned)", Private)
         ]

uploadDocumentForm
    :: Markup
    -> MForm (HandlerFor App) ((FormResult
                     (FileInfo, SharingScope, Maybe Textarea, UTCTime, Maybe [Text]),
                   WidgetFor App ()))
uploadDocumentForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
            <$> fileAFormReq (bfs ("Document" :: Text))
            <*> areq (selectFieldList scopes) (bfs ("Share With " :: Text)) Nothing
            <*> aopt textareaField (bfs ("Description"::Text)) Nothing
            <*> lift (liftIO getCurrentTime)
            <*> aopt tagField "Tags" Nothing

updateDocumentForm
    :: Markup
    -> MForm (HandlerFor App) ((FormResult
                     (String, Maybe SharingScope, Maybe Textarea, Maybe FileInfo,
                      Maybe [Text]),
                   WidgetFor App ()))
updateDocumentForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
            <$> areq docId "" Nothing
            <*> aopt (selectFieldList scopes) (bfs ("Share With " :: Text)) Nothing
            <*> aopt textareaField (bfs ("Description"::Text)) Nothing
            <*> fileAFormOpt (bfs ("Replacement File" :: Text))
            <*> aopt tagField "Tags" Nothing
    where docId :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m String
          docId = hiddenField


tagField :: Field Handler [Text]
tagField = Field
    { fieldParse = \raw _ -> case raw of [a] -> return $ Right $ Just (T.splitOn "," a);
                                         _ -> return $ Right Nothing
    , fieldView = \idAttr nameAttr _ _ _ ->
             [whamlet|
                <input id=#{idAttr} name=#{nameAttr} data-role="tagsinput">
             |]
    , fieldEnctype = UrlEncoded
    }

updateDocumentModal :: WidgetFor App () -> Enctype -> WidgetFor App ()
updateDocumentModal = genericModal "Document" "Update Shared Document"

setBookAssignmentForm
    :: [Entity Course]
    -> Markup
    -> MForm (HandlerFor App) ((FormResult
                     (Entity Course, Int, Day, Maybe TimeOfDay),
                   WidgetFor App ()))
setBookAssignmentForm classes extra = do
            (classRes, classView) <- mreq (selectFieldList classnames) (bfs ("Class" :: Text)) Nothing
            (probRes, probView) <- mreq (selectFieldList chapters) (bfs ("Problem Set" :: Text))  Nothing
            (dueRes, dueView) <- mreq (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Due Date"::Text)) Nothing
            (duetimeRes, duetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
            let theRes = (,,,) <$> classRes <*> probRes <*> dueRes <*> duetimeRes
            let widget = [whamlet|
                #{extra}
                <h6>Assign to
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput classView}
                <h6> Problem Set
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput probView}
                <h6> Due Date
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput dueView}
                    <div.form-group.col-md-6>
                        ^{fvInput duetimeView}
                |]
            return (theRes, widget)
    where chapters = map (\x -> ("Problem Set " ++ pack (show x),x)) [1..17] :: [(Text,Int)]
          classnames = map (\theclass -> (courseTitle . entityVal $ theclass, theclass)) classes

createCourseForm
    :: Markup
    -> MForm (HandlerFor App) ((FormResult
                     (Text, Maybe Textarea, Day, Day, TZLabel),
                   WidgetFor App ()))
createCourseForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
            <$> areq textField (bfs ("Title" :: Text)) Nothing
            <*> aopt textareaField (bfs ("Course Description"::Text)) Nothing
            <*> areq (jqueryDayField def) (bfs ("Start Date"::Text)) Nothing
            <*> areq (jqueryDayField def) (bfs ("End Date"::Text)) Nothing
            <*> areq (selectFieldList zones)    (bfs ("TimeZone"::Text)) Nothing
    where zones = map (\(x,y,_) -> (decodeUtf8 x,y)) (rights tzDescriptions)

updateOldCourseModal :: WidgetFor App () -> Enctype -> WidgetFor App ()
updateOldCourseModal = genericModal "OldCourse" "Update Course Data"

data UpdateCourse
    = UpdateCourse
      { ucCourseId  :: Text
      , ucDesc      :: Maybe Textarea
      , ucStartDay  :: Maybe Day
      , ucEndDay    :: Maybe Day
      , ucPoints    :: Maybe Int
      , ucEnrolOpen :: Maybe Bool
      , ucTextbook  :: Maybe (Key AssignmentMetadata)
      , ucLtiId     :: Maybe Text
      }

updateCourseForm
    :: Maybe (Entity Course) -> Maybe (CourseAutoreg) -> [Entity AssignmentMetadata] -> Markup
    -> MForm (HandlerFor App) (FormResult UpdateCourse, WidgetFor App ())
updateCourseForm mcourseent mautoreg asmd = renderBootstrap3 BootstrapBasicForm $ UpdateCourse
            <$> areq courseId "" mcid
            <*> aopt textareaField (bfs ("Course Description"::Text)) (maybe Nothing (Just . Just . Textarea) (mdesc))
            <*> aopt (jqueryDayField def) (bfs ("Start Date"::Text)) (localize mstart)
            <*> aopt (jqueryDayField def) (bfs ("End Date"::Text)) (localize mend)
            <*> aopt intField (bfs ("Total Points for Course"::Text)) (maybe Nothing (Just . Just) mpoints)
            <*> aopt checkBoxField checkFieldSettings (maybe (Just Nothing) (Just . Just) mopen)
            <*> aopt (selectField assignmentlist) textbookFieldSettings (maybe Nothing (Just . Just) mtext)
            <*> aopt textField (bfs ("LTI Autoregistration ID" :: Text)) (Just mautoregId)
    where courseId = hiddenField
          mcourse = entityVal <$> mcourseent
          mtext = mcourse >>= courseTextBook
          mdesc = mcourse >>= courseDescription
          mstart = courseStartDate <$> mcourse
          mend = courseEndDate <$> mcourse
          mzone = tzByLabel <$> (mcourse >>= fromTZName . courseTimeZone)
          mpoints = courseTotalPoints <$> mcourse
          mcid = T.pack . show . entityKey <$> mcourseent
          mopen = courseEnrollmentOpen <$> mcourse
          mautoregId = toStrict . decodeUtf8 . A.encode <$> (tripleFromDB <$> mautoreg)
          localize t = (Just . localDay) <$> (utcToLocalTimeTZ <$> mzone <*> t)
          assignmentlist = pure $ OptionList assignments (readMaybe . unpack)
          assignments = map toAssignmentOption asmd
          toAssignmentOption (Entity k a) = Option (assignmentMetadataTitle a) k (pack . show $ k)
          textbookFieldSettings = case mcourse of
            Just _ -> (bfs ("Textbook for Course" :: Text))
            Nothing -> FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Nothing
                , fsAttrs = [("style","display:none")]
                }
          checkFieldSettings = FieldSettings
            { fsLabel = maybe "" (const "Enrollment Open?") mcourse
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = maybe [("style","display:none")] (const [("style","margin-left:10px")]) mcourse
            }

createAPIKeyForm :: Markup -> MForm (HandlerFor App) (FormResult (), WidgetFor App ())
createAPIKeyForm = renderBootstrap3 BootstrapBasicForm $ pure ()

updateAccommodationForm
    :: Markup
    -> MForm (HandlerFor App) ((FormResult
                     (String, String, Maybe Int, Maybe Double, Maybe Int),
                   WidgetFor App ()))
updateAccommodationForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
            <$> areq courseId "" Nothing
            <*> areq userId "" Nothing
            <*> aopt intField (bfs ("Minutes Added to Timed Assignment Duration"::Text)) Nothing
            <*> aopt doubleField (bfs ("Timed Assignment Duration Multiplied By"::Text)) Nothing
            <*> aopt intField (bfs ("Hours added to Due Date"::Text)) Nothing
    where courseId = hiddenField
          userId = hiddenField

updateAccommodationModal :: WidgetFor App () -> Enctype -> WidgetFor App ()
updateAccommodationModal = genericModal "Accommodation" "Update Accommodation"

updateExtensionForm
    :: [Entity AssignmentMetadata]
    -> Markup
    -> MForm (HandlerFor App) ((FormResult
                     (Text, Key AssignmentMetadata, Day, Maybe TimeOfDay),
                   WidgetFor App ()))
updateExtensionForm asmd = renderBootstrap3 BootstrapBasicForm $ (,,,)
            <$> areq userId "" Nothing
            <*> areq (selectField assignmentlist) (bfs ("Assignment" :: Text)) Nothing
            <*> areq (jqueryDayField def) (withPlaceholder "Due Date" $ bfs ("Due Date"::Text)) Nothing
            <*> aopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
    where userId = hiddenField
          assignmentlist = pure $ OptionList assignments (readMaybe . unpack)
          assignments = map toAssignmentOption asmd
          toAssignmentOption (Entity k a) = Option (assignmentMetadataTitle a) k (pack . show $ k)

addCoInstructorForm
    :: [Entity UserData]
    -> String
    -> Markup
    -> MForm (HandlerFor App) ((FormResult (String, Maybe (Key InstructorMetadata)),
                   WidgetFor App ()))
addCoInstructorForm instructors cid extra = do
    (courseRes,courseView) <- mreq courseId "" Nothing
    (instRes, instView) <- mreq (selectFieldList $ map toItem instructors) (bfs ("Instructor" :: Text)) Nothing
    let theRes = (,) <$> courseRes <*> instRes
        widget = do
            [whamlet|
            #{extra}
            ^{fvInput courseView}
            <div.input-group>
                ^{fvInput instView}
            <div.input-group>
                <input.btn.btn-primary onclick="submitAddInstructor(this,'#{cid}')" value="Add Co-Instructor">
            |]
    return (theRes,widget)
    where courseId :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m String
          courseId = hiddenField

          toItem (Entity _ i) = (userDataLastName i ++ ", " ++ userDataFirstName i, userDataInstructorId i)

deleteModal :: Text -> [Entity AssignmentMetadata] -> WidgetFor App ()
deleteModal id asmd =
    [whamlet|
        <div class="modal fade" id="deleteModalFor#{id}" tabindex="-1" role="dialog" aria-labelledby="deleteModalFor#{id}Label" aria-hidden="true">
            <div class="modal-dialog modal-lg" role="document">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="deleteModalFor#{id}Label">Reset Access Tokens:
                        <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                          <span aria-hidden="true">&times;</span>
                    <div class="modal-body">
                        <table.table>
                            <thead>
                                <tr>
                                    <td>For Assignment
                                    <td>Token Issued
                                    <td>
                            <tbody>
                                $forall Entity k a <- asmd
                                    <tr id="token-row-#{jsonSerialize k}">
                                        <td>#{assignmentMetadataTitle a}
                                        <td>—
                                        <td>
                                            <a href="#" onclick="event.preventDefault()" >reset
    |]

classWidget :: [Entity UserData] -> Entity Course -> Maybe (CourseAutoreg) -> Handler Widget
classWidget instructors classent autoreg = do
       let cid = entityKey classent
           course = entityVal classent
           chash = pack . show . hash . courseTitle $ course
           mprobs = readAssignmentTable <$> courseTextbookProblems course :: Maybe (IntMap UTCTime)
       coInstructors <- runDB $ selectList [CoInstructorCourse ==. cid] []
       coInstructorUD <- mapM udByInstructorId (map (coInstructorIdent . entityVal) coInstructors)
       theInstructorUD <- entityVal <$> udByInstructorId (courseInstructor course)
       allUserData <- map entityVal <$> (runDB $ selectList [UserDataEnrolledIn ==. Just cid] [])
       asmd <- runDB $ selectList [AssignmentMetadataCourse ==. cid] []
       let allUids = map userDataUserId allUserData
       musers <- mapM (\x -> runDB (get x)) allUids
       let users = catMaybes musers
           numberOfUsers = length allUids
           usersAndData = zip users allUserData
           sortedUsersAndData = let lnOf (_, UserData {userDataLastName = ln}) = ln
                                    in sortBy (\x y -> compare (toLower . lnOf $ x) (toLower . lnOf $ y)) usersAndData
           updateExtensionModal = genericModal ("ext" <> chash) "Set Alternate Due Date"
           updateCourseModal = genericModal ("course" <> chash) "Update Course Data"
           deleteTokenModal = deleteModal ("del" <> chash) asmd
           maybeTb = courseTextBook course >>= (\tb -> lookup tb (map (\a -> (entityKey a, entityVal a)) asmd)) >>= pure . assignmentMetadataTitle
       (addCoInstructorWidget,enctypeAddCoInstructor) <- generateFormPost (identifyForm "addCoinstructor" $ addCoInstructorForm instructors (show cid))
       (updateExtensionWidget,enctypeUpdateExtension) <- generateFormPost (identifyForm "updateExtension" $ updateExtensionForm asmd)
       (updateCourseWidget,enctypeUpdateCourse)
           <- generateFormPost (identifyForm "updateCourse" (updateCourseForm (Just classent) autoreg asmd))
       return [whamlet|
                    ^{updateExtensionModal updateExtensionWidget enctypeUpdateExtension}
                    ^{updateCourseModal updateCourseWidget enctypeUpdateCourse}
                    ^{deleteTokenModal}
                    <h2>Assignments
                    <div.scrollbox>
                        <table.assignment.table.table-striped>
                            <thead>
                                <th style="cursor:pointer" onclick="sortByCol(this,0)">
                                    Assignment
                                    <i class="fa fa-sort" aria-hidden="true"></i>
                                <th style="cursor:pointer" onclick="sortByCol(this,1)">
                                    Due Date
                                    <i class="fa fa-sort" aria-hidden="true"></i>
                            <tbody>
                                $maybe probs <- mprobs
                                    $forall (set,due) <- Data.IntMap.toList probs
                                        <tr>
                                            <td>Problem Set #{show set}
                                            <td>#{dateDisplay due course}
                                $forall Entity _ a <- asmd
                                    <tr>
                                        <td>
                                            <a href=@{CourseAssignmentR (courseTitle course) (assignmentMetadataTitle a)}>
                                                #{assignmentMetadataTitle a}
                                        $maybe due <- assignmentMetadataDuedate a
                                            <td>#{dateDisplay due course}
                                        $nothing
                                            <td>No Due Date
                    <h2>Students
                    <div.scrollbox
                        data-studentnumber="#{show numberOfUsers}"
                        data-cid="#{jsonSerialize cid}">
                        <table.table.table-striped >
                            <thead>
                                <th style="cursor:pointer" onclick="sortByCol(this,0)">
                                    Registered Student
                                    <i class="fa fa-sort" aria-hidden="true"></i>
                                <th style="cursor:pointer" onclick="sortByCol(this,1)">
                                    Student Name
                                    <i class="fa fa-sort" aria-hidden="true"></i>
                                <th style="cursor:pointer" onclick="sortByCol(this,2)">
                                    Total Score
                                    <i class="fa fa-sort" aria-hidden="true"></i>
                                <th> Action
                            <tbody>
                                $forall (u, UserData {userDataUniversityId = uniid, userDataEmail = memail, userDataFirstName = fn, userDataLastName = ln, userDataUserId = uid}) <- sortedUsersAndData
                                    <tr#student-#{userIdent u}>
                                        <td>
                                            <a href=@{UserR (userIdent u)}>#{userIdent u}
                                        <td>
                                            #{ln}, #{fn}
                                        <td.async
                                            data-query="#{jsonSerialize $ QueryScores uid cid}"
                                            data-email="#{userIdent u}"
                                            data-fn="#{fn}"
                                            data-ln="#{ln}"
                                            data-uniid="#{maybe "?" id uniid}"
                                            data-uid="#{jsonSerialize uid}" >
                                            <span.loading>—
                                        <td>
                                            <button.btn.btn-sm.btn-secondary type="button" title="Drop #{fn} #{ln} from class"
                                                onclick="tryDropStudent('#{jsonSerialize $ DropStudent uid}')">
                                                <i.fa.fa-trash-o>
                                            $maybe email <- memail
                                                <button.btn.btn-sm.btn-secondary type="button" title="Email #{fn} #{ln}"
                                                    onclick="location.href='mailto:#{email}'">
                                                    <i.fa.fa-envelope-o>
                                            $nothing
                                                <button.btn.btn-sm.btn-secondary type="button" disabled title="No email address available for #{fn} #{ln}">
                                                    <i.fa.fa-envelope-o>
                                            <button.btn.btn-sm.btn-secondary type="button" title="Adjust Accessibility Settings for #{fn} #{ln}"
                                                onclick="modalEditAccommodation('#{show cid}','#{show uid}','#{jsonSerialize $ QueryAccommodation uid cid}')">
                                                <i.fa.fa-clock-o>
                                            <button.btn.btn-sm.btn-secondary type="button" title="Grant Extension to #{fn} #{ln}"
                                                onclick="modalGrantExtension(this,'#{"ext" <> chash}','#{jsonSerialize uid}')">
                                                <i.fa.fa-calendar-plus-o>
                                            <button.btn.btn-sm.btn-secondary type="button" title="Reset Exam Timer for #{fn} #{ln}"
                                                onclick="modalResetTimer('#{"del" <> chash}','#{jsonSerialize $ QueryTokens uid cid}')">
                                                <i.fa.fa-hourglass-start>
                    <h2>Course Data
                    <dl.row>
                        <dt.col-sm-3>Primary Instructor
                        <dd.col-sm-9>#{userDataLastName theInstructorUD}, #{userDataFirstName theInstructorUD}
                        <dt.col-sm-3>Course Title
                        <dd.col-sm-9>#{courseTitle course}
                        $maybe desc <- courseDescription course
                            <dd.col-sm-9.offset-sm-3>#{desc}
                        <dt.col-sm-3>Points Available
                        <dd.col-sm-9>#{courseTotalPoints course}
                        <dt.col-sm-3>Number of Students
                        <dd.col-sm-9>#{numberOfUsers} (Loaded:
                            <span id="loaded-#{jsonSerialize cid}"> 0#
                            )
                        <dt.col-sm-3>Start Date
                        <dd.col-sm-9>#{dateDisplay (courseStartDate course) course}
                        <dt.col-sm-3>End Date
                        <dd.col-sm-9>#{dateDisplay (courseEndDate course) course}
                        <dt.col-sm-3>Time Zone
                        <dd.col-sm-9>#{decodeUtf8 (courseTimeZone course)}
                        $maybe tbname <- maybeTb
                            <dt.col-sm-3>Custom Textbook
                            <dd.col-sm-9>#{tbname}
                        $maybe CourseAutoreg { courseAutoregLabel = mlab } <- autoreg
                            <dt.col-sm-3>Autoregistation From:
                            $maybe lab <- mlab
                                <dd.col-sm-9>#{lab}
                            $nothing
                                <dd.col-sm-9>Unlabled Course
                        <dt.col-sm-3>Enrollment Status
                        <dd.col-sm-9>
                            $if courseEnrollmentOpen course
                                Open
                            $else
                                Closed
                        <dt.col-sm-3>Enrollment Link
                        <dd.col-sm-9>
                            <a href="@{EnrollR (courseTitle course)}">@{EnrollR (courseTitle course)}
                        $if null coInstructors
                        $else
                            <dt.col-sm-3>Co-Instructors
                            <dd.col-sm-9>
                                $forall (Entity _ coud, Entity ciid _) <- zip coInstructorUD coInstructors
                                    <div#Co-Instructor-#{userDataLastName coud}-#{userDataFirstName coud}>
                                        <i.fa.fa-trash-o
                                            style="cursor:pointer"
                                            title="Remove #{userDataFirstName coud} #{userDataLastName coud} as Co-Instructor"
                                            onclick="tryDeleteCoInstructor('#{jsonSerialize $ DeleteCoInstructor ciid}','#{userDataLastName coud}', '#{userDataFirstName coud}')">
                                        <span>#{userDataFirstName coud},
                                        <span> #{userDataLastName coud}
                    <div.row>
                        <div.col-xl-6.col-lg-12 style="padding:5px">
                            <form.form-inline method=post enctype=#{enctypeAddCoInstructor}>
                                ^{addCoInstructorWidget}
                        <div.col-xl-6.col-lg-12 style="padding:5px">
                            <div.float-xl-right>
                                <button.btn.btn-secondary style="width:160px" type="button" onclick="modalEditCourse('#{"course" <> chash}')">
                                    Edit Information
                                <div.btn-group>
                                    <button.btn.btn-secondary.dropdown-toggle data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" style="width:160px" type="button">
                                        Export Grades
                                    <div.dropdown-menu aria-labelledby="export-button">
                                        <a.dropdown-item onclick="exportGrades('#{jsonSerialize cid}')";">
                                            Per Assignment
                                        <a.dropdown-item onclick="exportPerProblemGrades('#{jsonSerialize cid}')";">
                                            Per Problem
                                <button.btn.btn-danger style="width:160px" type="button" onclick="tryDeleteCourse('#{jsonSerialize $ DeleteCourse (courseTitle course)}')">
                                    Delete Course
              |]

dateDisplay :: UTCTime -> Course -> String
dateDisplay inUtc course = case tzByName $ courseTimeZone course of
                             Just tz  -> formatTime defaultTimeLocale "%F %R %Z" $ utcToZonedTime (timeZoneForUTCTime tz inUtc) inUtc
                             Nothing -> formatTime defaultTimeLocale "%F %R UTC" $ utc

maybeDo :: Monad m => Maybe t -> (t -> m ()) -> m ()
maybeDo mv f = case mv of Just v -> f v; _ -> return ()

-- TODO compare directory contents with database results
listAssignmentMetadata
    :: Entity Course
    -> (HandlerFor App [(Entity AssignmentMetadata, Entity Course)])
listAssignmentMetadata theclass = do asmd <- runDB $ selectList [AssignmentMetadataCourse ==. entityKey theclass] []
                                     return $ map (\a -> (a,theclass)) asmd
