--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Data.ByteString.Lazy as BL
import           Control.Applicative
import           Data.Csv
import qualified Data.Vector as V
import           Data.Time.Clock
import           Data.Time.Format
--------------------------------------------------------------------------------

data EventCategory = Normal | Permanent | Guerrilla deriving (Show)

data Event = Event
  { category :: !EventCategory
  , title :: !String
  , start :: !(Maybe UTCTime)
  , end :: !(Maybe UTCTime)
  , place :: !String
  , description :: !String
  , pictureName :: !String } deriving (Show)

instance FromNamedRecord Event where
  parseNamedRecord r = Event <$> (r .: "category" >>= (return . _parseEventCategory))
                             <*> r .: "title"
                             <*> (r .: "start" >>= (return . _parseDateString))
                             <*> (r .: "end" >>= (return . _parseDateString))
                             <*> r .: "place"
                             <*> r .: "description"
                             <*> r .: "pictureName"
    where
      _parseDateString "" = Nothing
      _parseDateString str = Just $ parseTimeOrError False defaultTimeLocale "%Y/%m/%d/%H%M" ("2016/" ++ str)
      _parseEventCategory :: String -> EventCategory
      _parseEventCategory str = case str of
        "0" -> Normal
        "1" -> Permanent
        "2" -> Guerrilla

extractDates :: String -> IO [Maybe UTCTime]
extractDates filepath = do
  csvData <- BL.readFile filepath
  let retval = V.toList $ case decodeByName csvData of
                            Left err -> error err
                            Right (_, vectors) -> V.map (\ r -> start r ) vectors
  return retval

main :: IO ()
main = do
  dates <- extractDates "data/table.csv"
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["index.html"] $ do
      rounte idRoute
      compile $ do
        let indexCtx =
          listField "dates" defaultContext (return dates) `mappend`
          defaultContext
{-
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
-}
