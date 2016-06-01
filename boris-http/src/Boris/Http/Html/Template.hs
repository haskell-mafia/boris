{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Html.Template (
    dashboard
  , projects
  , project
  , builds
  , build
  , render
  ) where

import           Airship (Webmachine, ResponseBody (..))

import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           BMX (BMXError, renderBMXError)
import           BMX (Template, renderPage, renderTemplate, templateFile)
import           BMX (BMXValue (..), defaultState, usingContext)

import           Boris.Core.Data (Project (..), Build (..), Ref (..), BuildId (..), BuildResult (..), sortBuildIds)
import           Boris.Store.Build (BuildData (..))
import           Boris.Http.Airship (webT)

import           Data.Time (UTCTime, diffUTCTime, formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (hoistEither)


render :: Either BMXError Text -> Webmachine IO ResponseBody
render t = do
  c <- webT renderBMXError . hoistEither $ t
  return . ResponseBuilder . fromByteString . T.encodeUtf8 $ c


dashboard :: Either BMXError Text
dashboard =
  let
    context = [
        ("constant", BMXString "THIS IS A CONSTANT")
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) dashboard'

projects :: [Project] -> Either BMXError Text
projects p =
  let
    context = [
        ("projects", BMXList ((BMXString . renderProject) <$> sortOn renderProject p))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) projects'

project :: Project -> [Build] -> Either BMXError Text
project p bs =
  let
    context = [
        ("project", BMXString (renderProject p))
      , ("builds", BMXList ((BMXString . renderBuild) <$> sortOn renderBuild bs))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) project'

builds :: Project -> Build -> [(Ref, [BuildId])] -> [BuildId] -> Either BMXError Text
builds p b rs queued =
  let
    prepped :: [(Text, BMXValue)]
    prepped =
      fmap (\(r, is) -> (renderRef r, BMXList $ (BMXString . renderBuildId) <$> sortBuildIds is)) rs

    context = [
        ("project", BMXString (renderProject p))
      , ("build", BMXString (renderBuild b))
      , ("refs", BMXContext prepped)
      , ("queued", BMXList ((BMXString . renderBuildId) <$> sortBuildIds queued))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) builds'

build :: BuildData -> Either BMXError Text
build b =
  let
    context = [
        ("project", BMXString (renderProject . buildDataProject $ b))
      , ("build", BMXString (renderBuild . buildDataBuild $ b))
      , ("id", BMXString (renderBuildId . buildDataId $ b))
      , ("ref", maybe BMXNull (BMXString . renderRef) . buildDataRef $ b)
      , ("queued", maybe BMXNull (BMXString . renderTime) . buildDataQueueTime $ b)
      , ("started", maybe BMXNull (BMXString . renderTime) . buildDataStartTime $ b)
      , ("ended", maybe BMXNull (BMXString . renderTime) . buildDataEndTime $ b)
      , ("hearbeat", maybe BMXNull (BMXString . renderTime) . buildDataHeartbeatTime $ b)
      , ("duration", maybe BMXNull (BMXString . uncurry renderDuration) $ liftA2 (,) (buildDataStartTime b) (buildDataEndTime b))
      , ("result", maybe BMXNull (BMXString . renderBuildResult) $ buildDataResult b)
      , ("ok", maybe BMXNull (BMXBool . (==) BuildOk) $ buildDataResult b)
      , ("ko", maybe BMXNull (BMXBool . (==) BuildKo) $ buildDataResult b)
      , ("undecided", maybe (BMXBool True) (const $ BMXNull) $ buildDataResult b)
      , ("log", maybe BMXNull (const $ BMXBool True) $ buildDataLog b)
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) build'

renderTime :: UTCTime -> Text
renderTime =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

renderDuration :: UTCTime -> UTCTime -> Text
renderDuration s e =
  mconcat [T.pack . show $ ((round (diffUTCTime e s)) :: Integer), "s"]

renderBuildResult :: BuildResult -> Text
renderBuildResult r =
  case r of
    BuildOk ->
      "ok"
    BuildKo ->
      "ko"

dashboard' :: Template
dashboard' =
  $(templateFile "template/dashboard.hbs")

projects' :: Template
projects' =
  $(templateFile "template/projects.hbs")

project' :: Template
project' =
  $(templateFile "template/project.hbs")

builds' :: Template
builds' =
  $(templateFile "template/builds.hbs")

build' :: Template
build' =
  $(templateFile "template/build.hbs")