module Model.Era where

import           Control.Monad
import           Data.Aeson
import           Data.KindID (KindID)
import qualified Data.KindID as KID
import           Data.Word
import           GHC.Generics
import           Model.Literal

import           Model.Dynasty hiding (Era)

-- | 年号的唯一标识符，使用TypeID来表示。
type EraID = KindID "era"

genEraID :: IO EraID
genEraID = KID.genKindID

genEraIDs :: Word16 -> IO [EraID]
genEraIDs = KID.genKindIDs

-- | 年号的基本信息。
data Era = Era
  { -- | 年号的唯一标识符。
    eraId    :: EraID
    -- | 年号的名称。
  , name     :: Literal
    -- | 年号的朝代。
  , dynasty  :: Literal
    -- | 年号的主君。
    --
    -- 众所周知，当新帝登基时，一般会沿用先帝的年号直到来年正月，因此一个年号往往会对应多位皇
    -- 帝。然而，习惯上我们并不会把沿用的年号算作是新帝的，因此我们使用“主君”表示该年号的主要
    -- 使用者。
    --
    -- 严格而言，“主君”的定义如下：
    --
    -- 1. 新帝登基后，如果在新年或者新年之前改元，则先帝为“主君”；
    -- 2. 新帝登基后，如果在新年之后改元，则创建一条新的年号记录，新帝为“主君”；
    -- 3. 在皇帝交替的间隔时期，以先帝为“主君”。这种情况一般不会出现太久，但是也有例外，如两晋
    --    和南明等。
  , norm     :: Literal
    -- | 年号的其他君主（从君）。
    --
    -- 除主君外所有使用该年号的君主。
  , emperors :: [Literal]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

bar :: IO ()
bar = do
  ds <- loadDynasty
  eras <- forM ds \d -> do
    let dn = d.name
    forM d.emperors \e -> do
      let en = e.name
      kids <- genEraIDs (fromIntegral $ length e.eras)
      forM (zip kids e.eras) \(kid, r) -> do
        pure $ Era kid r.name dn en []
  encodeFile "../data/eras.json" $ concatMap concat eras
