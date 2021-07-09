module Handler.Document where

import           Import

import           Data.List        (nub)
import           System.Directory (doesFileExist)
import           System.FilePath
import           Text.Julius      (juliusFile)
import           Text.Pandoc      (lookupMeta)
import           TH.RelativePaths (pathRelativeToCabalPackage)

import           Util.Data
import           Util.Database
import           Util.Handler

getDocumentsR :: Handler Html
getDocumentsR =  runDB (selectList [] []) >>= documentsList "Index of All Documents"

getDocumentsByTagR :: Text -> Handler Html
getDocumentsByTagR tag = do documents <- runDB $ do tags <- selectList [TagName ==. tag] []
                                                    docs <- mapM (get . tagBearer . entityVal) tags
                                                    return $ zipWith (\(Just d) t -> Entity (tagBearer $ entityVal t) d) docs tags
                            documentsList (toHtml $ "Documents tagged " <> tag) documents

documentsList :: Html -> [Entity Document] -> Handler Html
documentsList title documents = do
                   let publicDocuments = filter ((==) Public . documentScope . entityVal) documents
                   pubidents <- mapM (getIdent . documentCreator . entityVal) publicDocuments
                   pubmd <- mapM (getUserMD . documentCreator . entityVal) publicDocuments
                   maybeData <- maybeUserData
                   case userDataInstructorId <$> entityVal <$> maybeData of
                      Just _id -> do
                           let docs = filter ((==) InstructorsOnly . documentScope . entityVal) documents
                               privateDocuments = if null docs then Nothing else Just docs
                               allDocuments = publicDocuments ++ docs
                           tagMap <- forM allDocuments $ \doc -> do
                                            tags <- runDB $ selectList [TagBearer ==. entityKey doc] []
                                            return (entityKey doc, map (tagName . entityVal) tags)
                           let allTags = nub . concat . map snd $ tagMap
                           privmd <- mapM (getUserMD . documentCreator . entityVal) docs
                           prividents <- mapM (getIdent . documentCreator . entityVal) docs
                           defaultLayout $ do
                               setTitle title
                               $(widgetFile "documentIndex")
                      Nothing -> do let privateDocuments = Nothing
                                        (privmd, prividents) = ([],[])
                                    tagMap <- forM publicDocuments $ \doc -> do
                                                    tags <- runDB $ selectList [TagBearer ==. entityKey doc] []
                                                    return (entityKey doc, map (tagName . entityVal) tags)
                                    let allTags = nub . concat . map snd $ tagMap
                                    defaultLayout $ do
                                        setTitle $ title
                                        $(widgetFile "documentIndex")
    where documentCards docs idents mds tagMap = [whamlet|
    $forall (Entity k doc,mident, mmd) <- zip3 docs idents mds
        $maybe ident <- mident
            $maybe md <- mmd
                <div.card>
                    <div.card-header>
                        <a href=@{DocumentR ident (documentFilename doc)}>
                            #{documentFilename doc}
                    <div.card-block>
                        <dl.row>
                            <dt.col-sm-3> Title
                            <dd.col-sm-9> #{documentFilename doc}
                            $maybe desc <- documentDescription doc
                                <dt.col-sm-3> Description
                                <dd.col-sm-9> #{desc}
                            <dt.col-sm-3> Creator
                            <dd.col-sm-9> #{userDataFirstName md} #{userDataLastName md}
                            <dt.col-sm-3> Created on
                            <dd.col-sm-9> #{formatTime defaultTimeLocale "%F %R UTC" $ documentDate doc}
                            $maybe tags <- lookup k tagMap
                                <dt.col-sm-3> Tags
                                <dd.col-sm-9>
                                        $forall tag <- tags
                                            <a.badge.badge-primary href="@{DocumentsByTagR tag}">#{tag}
                        <button.btn.btn-sm.btn-secondary type="button" onclick="window.open('@{DocumentDownloadR ident (documentFilename doc)}')">
                            <i.fa.fa-cloud-download>
    |]

-- XXX DRY up the boilplate that is shared by getDocumentDownload
getDocumentR :: Text -> Text -> Handler Html
getDocumentR ident title = do (Entity _key doc, path, creatorid) <- retrieveDoc ident title
                              muid <- maybeAuthId
                              mud <- maybeUserData
                              let isNotInstructor = null (mud >>= userDataInstructorId . entityVal)
                              case documentScope doc of
                                --serveDoc bypasses the remaining handlers, so the not-found here is a placeholder that is never reached
                                 Private | Just creatorid /= muid -> setMessage "shared file for this document not found" >> notFound
                                 InstructorsOnly | isNotInstructor -> setMessage "shared file for this document not found" >> notFound
                                 _ | takeExtension path == ".css"  -> serveDoc asCss doc path creatorid >> notFound
                                   | takeExtension path == ".js"   -> serveDoc asJs doc path creatorid >> notFound
                                   | takeExtension path == ".yaml" -> serveDoc asYaml doc path creatorid >> notFound
                                   | takeExtension path == ".png"  -> sendFile typePng path >> notFound
                                   | takeExtension path == ".jpg"  -> sendFile typeJpeg path >> notFound
                                   | takeExtension path == ".jpeg" -> sendFile typeJpeg path >> notFound
                                   | takeExtension path == ".gif"  -> sendFile typeGif path >> notFound
                                   | takeExtension path == ".svg"  -> sendFile typeSvg path >> notFound
                                   | takeExtension path == ".pdf"  -> asFile doc path >> notFound
                                   | otherwise -> returnFile path
    where returnFile path = do
              ehtml <- liftIO $ fileToHtml allFilters path
              case ehtml of
                  Left err -> defaultLayout $ minimalLayout (show err)
                  Right (Left err,_) -> defaultLayout $ minimalLayout (show err)
                  Right (Right html, meta) -> do
                      mbcss <- retrievePandocVal (lookupMeta "base-css" meta)
                      mcss <- retrievePandocVal (lookupMeta "css" meta)
                      mjs <- retrievePandocVal (lookupMeta "js" meta)
                      let theLayout = \widget -> case mbcss of
                                       Nothing -> defaultLayout $ do mapM addStylesheet [StaticR css_bootstrapextra_css]
                                                                     widget
                                       Just bcss -> cleanLayout $ do mapM addStylesheetRemote bcss
                                                                     widget
                      theLayout $ do
                          toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/command.julius")
                          addDocScripts
                          maybe (pure [()]) (mapM addScriptRemote) mjs
                          maybe (pure [()]) (mapM addStylesheetRemote) mcss
                          $(widgetFile "document")

getDocumentDownloadR :: Text -> Text -> Handler TypedContent
getDocumentDownloadR ident title = do (Entity _key doc, path, creatoruid) <- retrieveDoc ident title
                                      serveDoc asFile doc path creatoruid

retrieveDoc :: Text -> Text -> Handler (Entity Document, FilePath, UserId)
retrieveDoc ident title = do userdir <- getUserDir ident
                             let path = userdir </> unpack title
                             exists <- liftIO $ doesFileExist path
                             mcreator <- runDB $ getBy $ UniqueUser ident
                             case mcreator of
                                 _ | not exists -> setMessage "shared file for this document not found" >> notFound
                                 Nothing -> setMessage "document creator not found" >> notFound
                                 Just (Entity creatoruid _) -> do
                                     mdoc <- runDB $ getBy (UniqueDocument title creatoruid)
                                     case mdoc of
                                         Nothing -> setMessage "metadata for this document not found" >> notFound
                                         Just doc -> return (doc, path, creatoruid)

getUserDir :: (MonadHandler m, MonoFoldable mono, HandlerSite m ~ App, Element mono ~ Char) => mono -> m FilePath
getUserDir ident = do master <- getYesod
                      return $ (appDataRoot $ appSettings master) </> "documents" </> unpack ident
