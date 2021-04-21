{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RestService (start) where

import qualified BizExpr as E
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import GHC.Generics
import qualified Network.HTTP.Types.Status as Status
import Network.HTTP.Types.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Web.Scotty as S

addHeaders :: Middleware
addHeaders =
  modifyResponse
    ( mapResponseHeaders
        ( [ ("Access-Control-Allow-Methods ", "GET, POST"),
            ("Access-Control-Allow-Headers", "Content-Type"),
            ("Access-Control-Allow-Origin", "*")
          ]
            ++
        )
    )

staticFilePaths :: [FilePath]
staticFilePaths = map ("./static/" ++) ["materialize.min.css", "materialize.min.js", "service.js"]

staticFiles :: ScottyM ()
staticFiles = do
  mapM_ addFile staticFilePaths
  where
    addFile path = get (capture $ tail path) $ file path

data ExprReq = ExprReq {expression :: L.Text, result :: Maybe L.Text}
  deriving (Show, Generic)

instance FromJSON ExprReq

instance ToJSON ExprReq

--numInc :: State -> IO Table
--numInc s = atomically $ (\x -> writeTVar s x >> return x) . () =<< readTVar s

getTable :: State -> Int -> IO (Maybe Table)
getTable s n = atomically $ IM.lookup n <$> readTVar s

setTable :: State -> Int -> Table -> IO (Maybe Table)
setTable s n t = atomically $ (\x -> writeTVar s x >> return (Just t)) . IM.insert n t =<< readTVar s

routes :: State -> ScottyM ()
routes state = do
  S.middleware addHeaders
  staticFiles
  get "/t/dummy" $
    S.json $ evalTableExpressions (TableExpressions dummyTable ["sum(2,3,5)", "any(0,0,1)", "sum(age, -10)", "any(0,1)"])
  get "/t/:id" $
    S.json =<< liftAndCatchIO . getTable state =<< S.param "id"
  post "/t/:id" $ do
    id <- S.param "id"
    S.json =<< liftAndCatchIO . setTable state id =<< S.jsonData
  get "/:expr" $
    S.text =<< S.param "expr"
  post "/eval" $
    S.json . (ExprReq <*> eval M.empty) . expression =<< S.jsonData

eval :: E.Context -> L.Text -> Maybe L.Text
eval c = fmap L.pack . E.eval c . L.unpack

type TableHeading = L.Text

type TableRow = M.Map TableHeading TableCell

type TableCell = L.Text

type Tables = IM.IntMap Table

type Expression = L.Text

data TableExpressions = TableExpressions
  { table :: Table,
    expressions :: [Expression]
  }
  deriving (Show, Generic)

evalTableExpressions :: TableExpressions -> Table
evalTableExpressions (TableExpressions table expressions) =
  foldl applyExpression table expressions

applyExpression :: Table -> Expression -> Table
applyExpression table expr =
  table
    { table_headings = table_headings table ++ [expr],
      table_rows =
        map
          ( \row ->
              M.insert
                expr
                ( fromMaybe "-" (eval row expr)
                )
                row
          )
          (table_rows table)
    }

data Table = Table
  { table_name :: L.Text,
    table_headings :: [TableHeading],
    table_rows :: [TableRow]
  }
  deriving (Show, Generic)

instance FromJSON Table

instance ToJSON Table

type State = TVar Tables

dummyTable =
  Table
    { table_name = "dummy table",
      table_headings = ["name", "age", "height"],
      table_rows =
        [ M.fromList
            [ ("name", "alice"),
              ("age", "30")
            ],
          M.fromList
            [ ("name", "bob"),
              ("age", "40")
            ],
          M.fromList
            [ ("name", "charles"),
              ("height", "1.80")
            ]
        ]
    }

addRow :: Table -> TableRow -> Table
addRow t tr = t {table_rows = tr0 : table_rows t}
  where
    tr0 = M.filterWithKey (\k _ -> k `elem` table_headings t) tr

stateInit :: IO State
stateInit = newTVarIO $ IM.fromList [(-1, dummyTable)]

start :: IO ()
start = scottyOpts opts . routes =<< stateInit

opts :: Options
opts = Options 1 settings
  where
    settings = setPort 3000 defaultSettings
