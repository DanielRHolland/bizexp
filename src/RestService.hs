{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module RestService (start) where

import Network.HTTP.Types.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty as S
import Data.Maybe (fromMaybe)
import GHC.Generics

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray

import qualified BizExpr as E
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Network.HTTP.Types.Status as Status

staticFilePaths :: [FilePath]
staticFilePaths = map ("./static/" ++) ["materialize.min.css", "materialize.min.js", "service.js"]

staticFiles :: ScottyM ()
staticFiles = do
  --    get "/static/:file" $ file
  mapM_ addFile staticFilePaths
  where
    addFile path = get (capture $ tail path) $ file path

data ExprReq = ExprReq {expression :: L.Text, result :: Maybe L.Text}
    deriving (Show, Generic)
instance FromJSON ExprReq

numInc :: State -> IO Int
numInc s = atomically (readTVar s >>= writeTVar s . (+1) >> readTVar s)

routes :: State -> ScottyM ()
routes state = do
  staticFiles
  get "/num" $
    S.json =<< liftAndCatchIO (numInc state)
  get "/:expr" $
    S.text =<< S.param "expr"
  post "/eval" $
    S.text . L.pack . fromMaybe "failure" . E.eval . L.unpack . expression =<< S.jsonData

type State = TVar Int

start :: IO ()
start = scottyOpts opts . routes =<< newTVarIO 3

opts :: Options
opts = Options 1 settings
  where
    settings = setPort 3000 defaultSettings
