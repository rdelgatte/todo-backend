{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Aeson as Scotty
import qualified Data.Text as T
import System.Random (newStdGen, randomRs)
import Types (Item (..), ItemSpec (..))
import Web.Scotty as S
import qualified Web.Scotty.Trans as Scotty
import Web.Scotty
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Database (migrateAll)


main :: IO ()
main = do
    let dbFile = "test.db"
    runSqlite dbFile $ do
        runMigration migrateAll
    scotty 3000 $ do
        -- To demonstrate that routes are matched top-down.
        S.get "/" $ text "foobar"
        S.get "/" $ text "barfoo"

        S.get "/random" $ do
            g <- liftIO newStdGen
            json $ take 20 $ randomRs (1 :: Int, 100) g


        S.get "/items" $ do
            json item
        where
            item = Item {identifier = Just 1, title = "test", description = Nothing, completed = False}