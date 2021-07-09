module Handler.Serve where

import           Import
import           System.Directory (doesFileExist)
import           System.FilePath  as FP (joinPath, takeExtension)
import           Text.Pandoc      (lookupMeta)
import           Util.Handler


-- XXX DRY up the boilplate that is shared by getDocumentDownload
getServeR :: Text -> [Text] -> Handler Html
getServeR base components = do app <- getYesod
                               let root = appDataRoot $ appSettings app
                               path <- case components of
                                         [] -> redirect $ ServeR base [pack "index.md"]
                                         _ -> return $ FP.joinPath (unpack root:"srv":unpack base:map unpack components)
                               liftIO (doesFileExist path) >>= \exists -> if exists then return () else notFound
                               case () of
                                   () | takeExtension path == ".css" -> sendFile typeCss path >> notFound
                                      | takeExtension path == ".js" -> sendFile typeJavascript path >> notFound
                                      | takeExtension path == ".yaml" -> sendFile "text/x-yaml; charset=utf-8" path >> notFound
                                      | takeExtension path == ".png" -> sendFile typePng path >> notFound
                                      | takeExtension path == ".html" -> sendFile typeHtml path >> notFound
                                      | takeExtension path == ".jpg" -> sendFile typeJpeg path >> notFound
                                      | takeExtension path == ".jpeg" -> sendFile typeJpeg path >> notFound
                                      | takeExtension path == ".gif" -> sendFile typeGif path >> notFound
                                      | takeExtension path == ".svg" -> sendFile typeSvg path >> notFound
                                      | otherwise -> returnFile path
    where returnFile path = do
              ehtml <- liftIO $ fileToHtml allFilters path
              case ehtml of
                  Left err -> defaultLayout $ minimalLayout (show err)
                  Right (Left err,_) -> defaultLayout $ minimalLayout (show err)
                  Right (Right html, meta) -> do
                      mcss <- retrievePandocVal (lookupMeta "css" meta)
                      mjs <- retrievePandocVal (lookupMeta "js" meta)
                      defaultLayout $ do
                          addDocScripts
                          maybe (pure [()]) (mapM (addScriptRemote))  mjs
                          case mcss of
                              Nothing -> mapM addStylesheet [StaticR css_bootstrapextra_css]
                              Just ss -> mapM addStylesheetRemote ss
                          $(widgetFile "document")
                          addScript $ StaticR ghcjs_allactions_runmain_js
