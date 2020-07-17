import Test.Tasty (defaultMain, testGroup)

import qualified Tests.EnumTH
import qualified Tests.GetQQ
import qualified Tests.MkGetterQQ
import qualified Tests.SchemaQQ
import qualified Tests.SumType
import qualified Tests.UnwrapQQ

main :: IO ()
main = defaultMain $ testGroup "aeson-schemas"
  [ Tests.GetQQ.test
  , Tests.UnwrapQQ.test
  , Tests.SchemaQQ.test
  , Tests.MkGetterQQ.test
  , Tests.EnumTH.test
  , Tests.SumType.test
  ]
