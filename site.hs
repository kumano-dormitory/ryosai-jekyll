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
isGuerrilla event = if category event == Guerrilla then True else False
isPermanent event = if category event == Permanent then True else False

showDay :: Day -> String
showDay day = formatTime defaultTimeLocale "%m月%d日" day

eventCtx :: Event -> Context String
eventCtx event = constField "description" (description event) <>
                 constField "pictureName" (pictureName event) <>
                 constField "title"       (title event)       <>
                 constField "period"      period              <>
                 constField "place"       (place event)       <>
                 defaultContext
                 where
                   startStr = case start event of
                     Just s -> formatTime defaultTimeLocale "%H:%M" s
                     Nothing -> ""
                   endStr = case end event of
                     Just e -> formatTime defaultTimeLocale "%H:%M" e
                     Nothing -> ""
                   period = if (startStr == "") && (endStr == "") then "？" else startStr ++ "〜" ++ endStr

eventsCtx :: [Event] -> String -> Context String
eventsCtx events _title = let items = forM events $ \event -> makeItem "" >>= loadAndApplyTemplate "templates/event.html" (eventCtx event)
                          in constField "title" _title <> listField "events" defaultContext items <> defaultContext

main :: IO ()
main = do
  events <- extractEvents "data/table.csv"
  let normalDays = extractDays . (filter isNormal) $ events
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
            listField "days" defaultContext (mapM (makeItem . showDay) normalDays) <>
            constField "title" "" <>
            defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
    
    forM normalDays $ \day -> do
      create [fromFilePath $ showDay day] $ do
        route   $ setExtension "html"
        compile $ do
          let eventsOfDay = filterByDay day $ filter isNormal events
              dayCtx = eventsCtx eventsOfDay ((showDay day) ++ "の企画")
          makeItem "" >>= loadAndApplyTemplate "templates/events.html" dayCtx
                      >>= loadAndApplyTemplate "templates/default.html" dayCtx
                      >>= relativizeUrls

    create ["guerrilla.html"] $ do
      route  idRoute
      compile $ do
        let guerrillaEvents = filter isGuerrilla events
            guerrillaCtx = eventsCtx guerrillaEvents "ゲリラ企画"
        makeItem "" >>= loadAndApplyTemplate "templates/events.html" guerrillaCtx
                    >>= loadAndApplyTemplate "templates/default.html" guerrillaCtx
                    >>= relativizeUrls

    create ["permanent.html"] $ do
      route  idRoute
      compile $ do
        let permanentEvents = filter isPermanent events
            permanentCtx = eventsCtx permanentEvents "常設企画"
        makeItem "" >>= loadAndApplyTemplate "templates/events.html" permanentCtx
                    >>= loadAndApplyTemplate "templates/default.html" permanentCtx
                    >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
