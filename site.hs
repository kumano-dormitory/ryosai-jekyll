--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Monoid (mappend, (<>))
import           Control.Monad (forM)
import           Hakyll
import qualified Data.ByteString.Lazy as BL
import           Control.Applicative
import           Data.Csv
import qualified Data.Vector as V
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Calendar
import           Data.Maybe
import           Data.List
import           Debug.Trace (trace)
--------------------------------------------------------------------------------

data EventCategory = Normal | Permanent | Guerrilla deriving (Show, Eq)

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

extractEvents :: String -> IO [Event]
extractEvents filepath = do
  csvData <- BL.readFile filepath
  return $ V.toList $ case decodeByName csvData of
                            Left err -> error err
                            Right (_, vector) -> vector

extractDays :: [Event] -> [Day]
extractDays events = sort . nub $ map (\(Just time) -> utctDay time) $ filter isJust $ map (\event -> start event) events 

filterByDay :: Day -> [Event] -> [Event]
filterByDay day events = filter (\event -> (Just . utctDay =<< start event) == Just day) $ filter (isJust . start) events

isNormal event = if category event == Normal then True else False

main :: IO ()
main = do
  events <- extractEvents "data/table.csv"
  let
    days = extractDays . (filter isNormal) $ events
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/picture/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["index.html"] $ do
      route idRoute
      compile $ do
        let
          indexCtx =
            listField "days" defaultContext (mapM (makeItem . show) days) `mappend`
            constField "title" "index" `mappend`
            defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
    
    forM days $ \day -> do
      create [fromFilePath $ show day] $ do
        route   $ setExtension "html"
        compile $ do
          let eventsOfDay = filterByDay day $ filter isNormal events
              items = forM eventsOfDay $ \event -> do
                let
                  eventCtx = constField "description" (description event) <>
                             constField "pictureName" (pictureName event) <>
                             defaultContext
                makeItem "" >>= loadAndApplyTemplate "templates/event.html" eventCtx
          let dayCtx = constField "day" (show day) <>
                       listField "events" defaultContext items <>
                       defaultContext
          makeItem "" >>= loadAndApplyTemplate "templates/day.html" dayCtx

    match "templates/*" $ compile templateBodyCompiler

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



--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
-}
