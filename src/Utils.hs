module Utils 
    (parseUsageError)
    where



import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as LBS
import           Servant




-- parseUsageError 
parseUsageError msg = throw500 (BS.pack $ show msg)

throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}