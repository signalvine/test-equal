{-# LANGUAGE OverloadedStrings, RankNTypes, KindSignatures, DataKinds,
             ConstraintKinds, GADTs, ScopedTypeVariables,
             DeriveFunctor, DeriveFoldable, DeriveTraversable,
             DefaultSignatures
             #-}
module Test.Equal
  ( -- * Types
    AreEqualF,
    Diff,
    AreEqual,
    Comparator,
    -- * Equal class
    Equal(..),
    -- * Comparators
    cmpAtom,
    cmpFields,
    cmpFieldsWith,
    -- * Pretty-printing
    ppEquality
  )
  where

import Prelude hiding (lines)
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Monad.Trans.State.Strict
import Generics.SOP

-- | Result of equality comparison
data AreEqualF diff
  = Equal
  | NotEqual diff
  deriving (Eq, Show, Functor, Foldable, Traversable)

type AreEqual = AreEqualF Diff

data Diff
  = ConstructorsDiffer
      DatatypeName
        (ConstructorName, String)
        (ConstructorName, String)
  | FieldsDiffer
      DatatypeName
      ConstructorName
      [(FieldName, Diff)]
  | AtomsDiffer
      String
      String
  deriving (Eq, Show)

cmpAtom :: (Eq a, Show a) => a -> a -> AreEqual
cmpAtom x y =
  if x == y
    then Equal
    else NotEqual $ AtomsDiffer (show x) (show y)

----------------------------------------------------------------------
--                           'Equal' class
----------------------------------------------------------------------

class Eq a => Equal a where
  cmp :: a -> a -> AreEqual

  default cmp :: Show a => a -> a -> AreEqual
  cmp = cmpAtom

instance Equal Bool
instance Equal Int
instance Equal Integer
instance Equal Char

----------------------------------------------------------------------
--                           Generic compare
----------------------------------------------------------------------

newtype Comparator a = Comparator { applyComparator :: a -> a -> AreEqual }

std :: forall xss . (All2 Equal xss, SingI xss) => POP Comparator xss
std = hcpure (Proxy :: Proxy Equal) $ Comparator cmp

cmpFields
  :: forall a xss .
     ( Generic a, xss ~ Code a
     , All2 Equal xss, SingI xss
     , HasDatatypeInfo a, Show a)
  => a -> a -> AreEqual
cmpFields = cmpFieldsWith std

cmpFieldsWith
  :: forall a . (Generic a, HasDatatypeInfo a, Show a)
  => POP Comparator (Code a)
  -> (a -> a -> AreEqual) -- ^ comparator
cmpFieldsWith cmpss_ x y = checkSum sing conInfos cmpss_ (from x) (from y)
  where
    conInfos :: NP ConstructorInfo (Code a)
    tyName :: DatatypeName
    (tyName, conInfos) =
      case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tyName' conInfos' -> (tyName', conInfos')
        Newtype _ tyName' conInfo -> (tyName', conInfo :* Nil)

    -- check that the two values are built from the same constructor of
    -- a sum type
    checkSum
      :: forall xss .
         Sing xss
      -> NP ConstructorInfo xss
      -> POP Comparator xss
      -> SOP I xss
      -> SOP I xss
      -> AreEqual
    checkSum SCons (c :*  _) (POP (cmps :* _)) (SOP (Z xs))  (SOP (Z ys))  = checkProduct c cmps xs ys
    checkSum SCons (_ :* cs) (POP (_:* cmpss)) (SOP (S xss)) (SOP (S yss)) = checkSum sing cs (POP cmpss) (SOP xss) (SOP yss)
    
    checkSum SCons (c :* cs) _ (SOP (Z _)) (SOP (S xs)) =
      NotEqual $ ConstructorsDiffer tyName
        (conName c, show x)
        (nthConstructorName xs cs, show y)
    checkSum SCons (c :* cs) _ (SOP (S xs)) (SOP (Z _)) =
      NotEqual $ ConstructorsDiffer tyName
        (nthConstructorName xs cs, show x)
        (conName c, show y)

    checkSum _ _ _ _ _ = error "Test.Equal.checkSum: non-exhaustive match?"

    -- checkProduct :: forall 
    checkProduct
      :: forall xs . SingI xs
      => ConstructorInfo xs
      -> NP Comparator xs
      -> NP I xs
      -> NP I xs
      -> AreEqual
    checkProduct ci cmps xs ys =
      fmap (FieldsDiffer tyName (conName ci)) . merge . hcollapse $
        hpure (fn_4 compareField)
        `hap` (fieldNames ci)
        `hap` cmps
        `hap` xs
        `hap` ys
  
merge :: [AreEqualF d] -> AreEqualF [d]
merge =
  let f :: AreEqualF  d
        -> AreEqualF [d]
        -> AreEqualF [d]
      f Equal diffs = diffs
      f (NotEqual d) Equal = NotEqual [d]
      f (NotEqual d) (NotEqual diffs) = NotEqual (d:diffs)
  in foldr f Equal

compareField
  :: FieldInfo x
  -> Comparator x
  -> I x
  -> I x
  -> K (AreEqualF (FieldName, Diff)) x
compareField (FieldInfo name) c (I x1) (I x2) =
  K $ fmap ((,) name) $ applyComparator c x1 x2

fieldNames :: SingI xs => ConstructorInfo xs -> NP FieldInfo xs
fieldNames ci =
  case ci of
    Record _ fields -> fields
    _ -> genNames

  where
    genNames :: SingI xs => NP FieldInfo xs
    genNames =
      evalState (hsequence' $ hpure (Comp genName)) 1

    genName :: SingI xs => State Int (FieldInfo xs)
    genName = do
      n <- get
      put $! n+1
      return $ FieldInfo (show n)

conName :: ConstructorInfo xs -> ConstructorName
conName ci =
  case ci of
    Constructor name -> name
    Infix name _ _ -> name
    Record name _ -> name

nthConstructorName :: SingI xs => NS smth xs -> NP ConstructorInfo xs -> ConstructorName
nthConstructorName n conInfos =
  unI . hcollapse $ hliftA2 (\ci _ -> K $ conName ci) conInfos n

----------------------------------------------------------------------
--                           Pretty-printing
----------------------------------------------------------------------

type Lines = [Builder]

indent :: Lines -> Lines
indent = map ("  " <>)

ppDiffL :: Diff -> Lines
ppDiffL diff =
  case diff of
    ConstructorsDiffer ty (con1, _) (con2, _) ->
      ("values of type " <> fromString ty <> " have different constructors:") : indent
      ["expected " <> fromString con2
      ,"but got  " <> fromString con1
      ]
    FieldsDiffer ty con diffs ->
      ("values of type " <> fromString ty <> " and constructor " <> fromString con <>
       " differ:") : (indent . concat)
      [ ("field " <> fromString field) : indent (ppDiffL diff')
      | (field, diff') <- diffs
      ]
    AtomsDiffer v1 v2 ->
      ["expected " <> fromString v1
      ,"but got  " <> fromString v2
      ]

ppEquality :: AreEqual -> Text
ppEquality eq =
  case eq of
    Equal -> "equal"
    NotEqual diff -> toLazyText . mconcat . map (<> "\n") $ ppDiffL diff
