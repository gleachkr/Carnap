module Handler.Chapter where

import           Import

import           Control.Monad.State    (evalState)
import qualified Data.CaseInsensitive   as CI
import           Data.Char              (isDigit)
import qualified Data.Text.Encoding     as TE
import           System.Directory       (getDirectoryContents)
import           Text.Hamlet            (hamletFile)
import           Text.Julius            (juliusFile)
import           Text.Pandoc
import           Text.Pandoc.Walk       (walk, walkM)
import           TH.RelativePaths       (pathRelativeToCabalPackage)
import           Yesod.Markdown

import           Filter.CounterModelers
import           Filter.ProofCheckers
import           Filter.Qualitative
import           Filter.Sidenotes
import           Filter.SynCheckers
import           Filter.Translate
import           Filter.TruthTables
import           Util.Database
import           Util.Handler           (addDocScripts)

-- XXX Fair amount of code-duplication between this and Handler/Book.hs. Perhaps merge those modules.

getChapterR :: Int -> Handler Html
getChapterR n = do bookdir <- appBookRoot <$> (appSettings <$> getYesod)
                   cdir <- liftIO $ getDirectoryContents bookdir
                   content' <- liftIO $ content n cdir bookdir
                   case content' of
                       Right (Right html) -> chapterLayout
                            [whamlet|
                                <div.container>
                                    <article>
                                        #{html}
                                        <nav.nextAndPrev>
                                            <p>
                                                $if (n > 1)
                                                    <a href="@{ChapterR (n - 1)}">
                                                        Previous Chapter
                                                $if ((n > 1) && (n < (length cdir - 3)))
                                                    <span>∙
                                                $if (n < (length cdir - 3))
                                                    <a href="@{ChapterR (n + 1)}">
                                                        Next Chapter
                            |]

                       Right (Left err) -> defaultLayout
                                      [whamlet|
                                        <div.container>
                                            <article>
                                                #{show err}
                                       |]
                       Left err -> defaultLayout
                                      [whamlet|
                                        <div.container>
                                            <article>
                                                #{show err}
                                       |]

content :: Int -> [FilePath] -> FilePath -> IO (Either PandocError (Either PandocError Html))
content n cdir cdirp = do let matches = filter (\x -> (show n ++ ".pandoc") == dropWhile (not . isDigit) x) cdir
                          case matches of
                              [] -> do print ("no matches"::Text)
                                       fileToHtml cdirp ""
                              (m:_)  -> fileToHtml cdirp m

fileToHtml :: FilePath -> FilePath -> IO (Either PandocError (Either PandocError Html))
fileToHtml path m = do md <- markdownFromFile (path </> m)
                       case parseMarkdown yesodDefaultReaderOptions { readerExtensions = exts } md of
                           Right pd -> do let pd' = applyFilters pd
                                          return $ Right $ writePandocTrusted yesodDefaultWriterOptions { writerExtensions = exts } pd'
                           Left e -> return $ Left e
    where exts = extensionsFromList
                    [ Ext_raw_html
                    , Ext_markdown_in_html_blocks
                    , Ext_auto_identifiers
                    , Ext_tex_math_dollars
                    , Ext_fenced_code_blocks
                    , Ext_backtick_code_blocks
                    , Ext_line_blocks
                    , Ext_fancy_lists
                    , Ext_definition_lists
                    , Ext_example_lists
                    , Ext_simple_tables
                    , Ext_multiline_tables
                    , Ext_footnotes
                    , Ext_fenced_code_attributes
                    ]

applyFilters= let walkNotes y = evalState (walkM makeSideNotes y) 0
                  walkProblems y = walk (makeSynCheckers . makeProofChecker . makeTranslate . makeTruthTables . makeCounterModelers . makeQualitativeProblems) y
                  in walkNotes . walkProblems

chapterLayout :: ToWidget App a => a -> Handler Html
chapterLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        authmaybe <- maybeAuth
        mud <- maybeUserData
        mcourse <- maybeUserCourse
        mdoc <- maybeUserTextbookDoc
        let isInstructor = not $ null (mud >>= userDataInstructorId . entityVal)
        pc <- widgetToPageContent $ do
            toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/command.julius")
            toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/status-warning.julius")
            toWidgetHead [julius|var submission_source="book";|]
            addStylesheet $ StaticR css_tufte_css
            addStylesheet $ StaticR css_tuftextra_css
            $(widgetFile "default-layout")
            addDocScripts

        withUrlRenderer $(hamletFile =<< pathRelativeToCabalPackage "templates/default-layout-wrapper.hamlet")
