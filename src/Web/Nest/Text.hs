module Web.Nest.Text
  ( bs2t
  , int2bs
  ) where

import qualified Data.ByteString.Char8 as S8
import           Data.Text             as T

bs2t :: S8.ByteString -> T.Text
bs2t = T.pack . S8.unpack

int2bs :: Int -> S8.ByteString
int2bs = S8.pack . show
