{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies #-}
import Generics.SOP
import Generics.SOP.TH
import Test.Equal
import Test.Tasty
import Test.Tasty.HUnit

data Type
  = Con1 { getCon1 :: Int }
  | Con2 Bool Char
  deriving (Eq, Show)

deriveGeneric ''Type

main = defaultMain $ testGroup "cmpFields"
  [ testCase "Equal" $
      cmpFields (Con1 2) (Con1 2) @?= Equal
  , testCase "Not equal (different constructors)" $
      cmpFields (Con1 2) (Con2 False 'a') @?=
        NotEqual (ConstructorsDiffer "Type" ("Con1","Con1 {getCon1 = 2}") ("Con2","Con2 False 'a'"))
  , testCase "Not equal (different constructors, opposite order)" $
      cmpFields (Con2 False 'a') (Con1 2) @?=
        NotEqual (ConstructorsDiffer "Type" ("Con2","Con2 False 'a'") ("Con1","Con1 {getCon1 = 2}"))
  , testCase "Not equal (same constructor, with field name)" $
      cmpFields (Con1 2) (Con1 3) @?=
        NotEqual (FieldsDiffer "Type" "Con1" [("getCon1",AtomsDiffer "2" "3")])
  , testCase "Not equal (same constructor, no field names)" $
      cmpFields (Con2 True 'a') (Con2 False 'a') @?=
        NotEqual (FieldsDiffer "Type" "Con2" [("1",AtomsDiffer "True" "False")])
  , testCase "Not equal (same constructor, no field names, 2 fields differ)" $
      cmpFields (Con2 True 'a') (Con2 False 'b') @?=
        NotEqual (FieldsDiffer "Type" "Con2" [("1",AtomsDiffer "True" "False"), ("2",AtomsDiffer "'a'" "'b'")])
  ]
