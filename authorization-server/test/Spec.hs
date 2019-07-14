import Test.HUnit

import Controllers.AuthorizeSpec
import Models.ClientSpec

main :: IO Counts
main = runTestTT . TestList $ (authorizeSpecTests ++ clientSpecTests)
