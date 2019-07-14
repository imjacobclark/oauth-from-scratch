import Test.HUnit

import AuthorizeSpec
import Models.ClientSpec

main :: IO Counts
main = runTestTT . TestList $ (authorizeSpecTests ++ clientSpecTests)
