import Test.HUnit

import AuthorizeSpec

main :: IO Counts
main = runTestTT (TestList authorizeSpecTests)
