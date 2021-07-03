{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Util.Data
import TH.RelativePaths (pathRelativeToCabalPackage)
import Carnap.GHCJS.SharedTypes(ProblemSource(..),ProblemType(..),ProblemData(..), SomeRule(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings =<< (pathRelativeToCabalPackage "config/models"))

--we put these in because the template haskell chokes on the keyword "data"
--as the name of a field
deriving instance Generic ProblemSubmission
deriving instance Generic (Key ProblemSubmission)
instance ToJSON ProblemSubmission
instance ToJSON (Entity ProblemSubmission)
instance FromJSON ProblemSubmission
instance FromJSON (Entity ProblemSubmission)

--And this one because ByteStrings (tzlabels) don't have a ToJSON
instance ToJSON Course where
        toJSON c = object [ "title" .= courseTitle c
                          , "description" .= courseDescription c
                          , "instructor" .= courseInstructor c
                          , "textbookProblems" .= courseTextbookProblems c
                          , "startDate" .= courseStartDate c
                          , "endDate" .= courseEndDate c
                          , "totalPoints" .= courseTotalPoints c
                          , "textBook" .= courseTextBook c
                          , "enrollmentOpen" .= courseEnrollmentOpen c
                          , "timeZone" .= (decodeUtf8 $ courseTimeZone c :: Text)
                          ]
