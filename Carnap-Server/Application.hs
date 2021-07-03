{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
import Database.Persist.Sqlite              (createSqlitePool)
import Import
import System.Clock
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware, pathInfo)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Throttle
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Serve
import Handler.Common
import Handler.Home
import Handler.Info
import Handler.Chapter
import Handler.Book
import Handler.User
import Handler.Command
import Handler.Register
import Handler.Rule
import Handler.Instructor
import Handler.Admin
import Handler.Assignment
import Handler.Document
import Handler.Review
import Handler.API.API
import Handler.API.Instructor.Documents
import Handler.API.Instructor.Students
import Handler.API.Instructor.Assignments
import Handler.API.Instructor.Courses


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    --Create the database connection pool
    pool <- flip runLoggingT logFunc $ 
                if appSqlite appSettings 
                    then createSqlitePool (pack (appDataRoot appSettings </> "sqlite.db")) 10
                    else createPostgresqlPool
                        (pgConnStr  $ appDatabaseConf appSettings)
                        (pgPoolSize $ appDatabaseConf appSettings)

    -- TODO: sqlite migrations are broken because persistent is not setting
    -- stuff properly because of https://github.com/yesodweb/persistent/issues/1125
    -- also relevant, https://github.com/yesodweb/persistent/issues/1126
    --
    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    let expirationSpec = TimeSpec 5 0
        zipsettings = if appDevel (appSettings foundation) 
                        then GzipIgnore
                        else GzipPreCompressed GzipCompress
        throttleSettings = (defaultThrottleSettings expirationSpec) 
                                { throttleSettingsIsThrottled = \req -> 
                                    case pathInfo req of
                                        "api":_ -> True
                                        _ -> False
                                , throttleSettingsRate = 2 --two requests per IP per second
                                , throttleSettingsBurst = 4 --four total requests per second
                                }
    -- Create the WAI application and apply middlewares
    logWare <- makeLogWare foundation
    throttler <- initThrottler throttleSettings
    appPlain <- toWaiAppPlain foundation
    return $ logWare 
           . throttle throttler
           . acceptOverride 
           . autohead 
           . gzip (def {gzipFiles = zipsettings }) 
           . methodOverride 
           . simpleCors $ appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromHeader)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

-- | Warp settings for the given foundation value.
-- behind https.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
