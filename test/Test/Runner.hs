import Test.Tasty
import qualified Test.Servant.PY.Golden

main :: IO ()
main = defaultMain $ testGroup "all" tests
  where
    tests =  Test.Servant.PY.Golden.tests