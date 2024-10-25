module RuFormatTests where

import Test.Hspec

import RuFormatModule
import RuVmModule

import qualified Data.ByteString as B
import Data.Either

spec :: Spec
spec = do
    describe "Combine n WordX into a WordY" $ do
        it "Combine 2 Word8 into a Word16" $ do
            (word82ToWord16 0xab 0xcd) `shouldBe` 0xabcd
        it "Combine 4 Word8 into a Word32" $ do
            (word84ToWord32 0x89 0xab 0xcd 0xef) `shouldBe` 0x89abcdef
    describe "RuFileNameToRuFormat" $ do
        it "Read a RuFormat from a file" $ do
            ruFormatResult <- fileNameToRuFormat "test/test.ru"
            case ruFormatResult of
                Left err -> expectationFailure $ ("Failed with error: " ++ show err)
                Right ruFormat -> ruFormat `shouldNotBe` defaultRuFormat
    describe "fileContentToRuFormat" $ do
                -- it "Returns an error for invalid magic number" $ do
                --     let invalidContent = B.pack [0x00, 0x00, 0x00, 0x00] -- Invalid magic number
                --     let result = fileContentToRuFormat (B.unpack invalidContent)
                --     result `shouldSatisfy` isLeft

                it "Parses valid RU format content" $ do
                    let validContent = B.pack [0x43, 0x52, 0x4f, 0x55, 0x53, -- Magic -- 5
                                               0x0a, 0x01, -- Checksum -- 7
                                               0x01, -- File version -- 8
                                               0x00, 0x00, 0x00, 0x02, -- function table count -- 12
                                               0x00, 0x00, 0x00, 0x58, -- String table offset -- 16
                                               0x00, 0x00, 0x00, 0x02, -- String table count -- 20
                                               0x00, 0x00, 0x00, 0x6d, -- Code offset -- 24
                                               0x00, 0x00, 0x00, 0x00, -- Entry point offset -- 28

                                               0x00, 0x00, 0x00, 0x00, --Unused --32
                                               0x00, 0x00, 0x00, 0x00, --Unused --36
                                               0x00, 0x00, 0x00, 0x00, --Unused --40
                                               0x00, 0x00, 0x00, 0x00, --Unused --44
                                               0x00, 0x00, 0x00, 0x00, --Unused --48
                                               0x00, 0x00, 0x00, 0x00, --Unused --52
                                               0x00, 0x00, 0x00, 0x00, --Unused --56
                                               0x00, 0x00, 0x00, 0x00, --Unused --60
                                               0x00, 0x00, 0x00, 0x00, --Unused --64
                                               -- Functions table
                                               0x00, 0x00, 0x00, 0x01, -- Name index -- 68
                                               0x00, 0x00, 0x00, 0x0e, -- Code offset -- 72
                                               0x00, 0x00, 0x00, 0xff, -- Code size -- 76

                                               0x00, 0x00, 0x00, 0x02, -- Name index -- 80
                                               0x00, 0x00, 0x00, 0x23, -- Code offset -- 84
                                               0x00, 0x00, 0x00, 0xff, -- Code size -- 88
                                               -- String table
                                               0x00, -- String 1 -- 89
                                               0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x31, 0x00, -- String 2 -- 99
                                               0x66, 0x75, 0x6e, 0x93, 0x74, 0x69, 0x6f, 0x6e, 0x32, 0x00, -- String 3 -- 109
                                               0x01, 0x00, -- Code section -- 111
                                               0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, -- Code section -- 118
                                               0x01, 0x02, 0x02, 0x02, 0x02, 0x01, 0x00, -- Code section -- 125
                                               0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, -- Code section -- 132
                                               0x01, 0x02, 0x02, 0x02, 0x02, 0x01, 0x00, -- Code section -- 139
                                               0x01, 0x03, 0x03, 0x03, 0x03, 0x01, 0x00, -- Code section -- 146
                                               0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, -- Code section -- 153
                                               0x01, 0x02, 0x02, 0x02, 0x02, 0x01, 0x00, -- Code section -- 160
                                               0x01, 0x03, 0x03, 0x03, 0x03 -- Code section -- 165
                                               ]
                    let result = fileContentToRuFormat (B.unpack validContent)
                    case result of
                        Left err -> expectationFailure $ ("Failed with error: " ++ show err)
                        Right ruFormat -> do
                            let header = ruHeader ruFormat
                            fileVersion header `shouldBe` 0x01
                            functionTableCount header `shouldBe` 0x00000002
                            strTableOffset header `shouldBe` 0x00000058
                            strTableCount header `shouldBe` 0x000000002
                            codeOffset header `shouldBe` 0x0000006d
                            entrypointOffset header `shouldBe` 0x00000000

                            -- let header = ruHeader defaultRuFormat
                            -- ruHeader ruFormat `shouldBe` header
                            ruFunctionTable ruFormat `shouldBe` [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x23, 0x00, 0x00, 0x00, 0xff]
                            strTab ruFormat `shouldBe` [0,102,117,110,99,116,105,111,110,49,0,102,117,110,147,116,105,111,110,50,0]
                            codeSection ruFormat `shouldBe` [1,0,1,1,1,1,1,1,0,1,2,2,2,2,1,0,1,1,1,1,1,1,0,1,2,2,2,2,1,0,1,3,3,3,3,1,0,1,1,1,1,1,1,0,1,2,2,2,2,1,0,1,3,3,3,3]

