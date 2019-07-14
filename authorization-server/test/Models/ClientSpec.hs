module Models.ClientSpec where

import Test.HUnit

import Controllers.Authorize
import Models.Client

testWithReadStringToReadScopeType = TestCase (assertEqual "returns Read scope type when read string is input" Read (scopeStringToScopeType "read"))
testWithReadStringToWriteScopeType = TestCase (assertEqual "returns Write scope type when write string is input" Write (scopeStringToScopeType "write"))

testWithEmptyStringToEmptyList = TestCase (assertEqual "returns [] when input is empty string" [] (scopeStringToScopeTypeList ""))
testWithJustReadStringToReadScopeType = TestCase (assertEqual "returns [Read] when input is read string" [Read] (scopeStringToScopeTypeList "read"))
testWithJustWriteStringToWriteScopeType = TestCase (assertEqual "returns [Write] when input is write string" [Write] (scopeStringToScopeTypeList "write"))
testWithReadWriteStringToReadAndWriteScopeType = TestCase (assertEqual "returns [Read, Write] when input is read write string" [Read, Write] (scopeStringToScopeTypeList "read write"))
testWithWriteReadStringToWriteAndReadScopeType = TestCase (assertEqual "returns [Write, Read] when input is write read string" [Write, Read] (scopeStringToScopeTypeList "write read"))

clientSpecTests = [
    TestLabel "testWithReadStringToReadScopeType" testWithReadStringToReadScopeType,
    TestLabel "testWithReadStringToWriteScopeType" testWithReadStringToWriteScopeType,
    TestLabel "testWithEmptyStringToEmptyList" testWithEmptyStringToEmptyList,
    TestLabel "testWithJustReadStringToReadScopeType" testWithJustReadStringToReadScopeType,
    TestLabel "testWithJustWriteStringToWriteScopeType" testWithJustWriteStringToWriteScopeType,
    TestLabel "testWithReadWriteStringToReadAndWriteScopeType" testWithReadWriteStringToReadAndWriteScopeType,
    TestLabel "testWithWriteReadStringToWriteAndReadScopeType" testWithWriteReadStringToWriteAndReadScopeType
    ]