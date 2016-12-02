{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
module Bokunonumachi() where

import Data.Kind
import Data.Proxy
import Data.Singletons -- stack install singletons
import Data.Singletons.TypeLits
import Data.Type.Equality
import GHC.TypeLits

data Matrix (row :: Nat) (col :: Nat) = Matrix { row :: Sing row, col :: Sing col }

instance Show (Matrix (row :: Nat) (col :: Nat)) where
  show m = show (fromSing $ row m) ++ " x " ++ show (fromSing $ col m)

prod :: Matrix a b -> Matrix b c -> Matrix a c
prod m1 m2 = Matrix (row m1) (col m2)

--readMatrix :: IO SomeMatrix

{-
AgdaのData.Product.Σを可能な限りそれっぽく移植
record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    proj₁ : A
    proj₂ : B proj₁
-}
data Sigma a (b :: a -> Data.Kind.*) where
  Sigma :: SingKind a =>
           { proj1 :: Sing (x :: a)
           , proj2 :: b x
           } -> Sigma a b

{-
AgdaのData.Product.∃
∃ : ∀ {a b} {A : Set a} → (A → Set b) → Set (a ⊔ b)
∃ = Σ _
-}
type Exist t = Sigma k (t :: k -> Data.Kind.*)

-- Sigmaの1要素目を参照して何かする
withProj1 :: Sigma a b -> (DemoteRep a -> r) -> r
withProj1 (Sigma {proj1 = p}) f = f . fromSing $ p

-- Sigmaの2要素目を参照して何かする，xは外に持ち出せない
withProj2 :: Sigma a b -> (forall x . b (x :: a) -> r) -> r
withProj2 (Sigma {proj2 = p}) f = f p

-- (型レベル)0と等しい(型レベルの)自然数値が，なにかしら存在するという命題の型
existsNatEqualsZero :: Exist ((:~:) 0) -- Sigma Nat ((:~:) 0) と同じ
-- と，その証明
existsNatEqualsZero = Sigma SNat Refl
-- existsNatEqualsZero = Sigma (SNat :: Sing 0) (Refl :: 0 :~: 0) -- 型注釈を付けると
-- existsNatEqualsZero = Sigma (SNat :: Sing 1) (Refl :: 0 :~: 1) -- 間違った型の値だと型エラー

-- 「(型レベル)0と等しい(型レベルの)値」を値レベルで取り出したもの
value :: Integer
value = withProj1 existsNatEqualsZero id

-- 「(型レベル)0と等しい(型レベルの)値」であるvalueが0と等しい証明
proof :: 0 :~: 0
proof = withProj2 existsNatEqualsZero f where
  f :: 0 :~: x -> 0 :~: 0
  f Refl = Refl

-- 形を隠す
newtype SomeMatrix' row = SomeMatrix' (Exist (Matrix row))
newtype SomeMatrix = SomeMatrix (Exist SomeMatrix')

withSomeMatrix :: SomeMatrix -> (forall row col . Matrix row col -> r) -> r
withSomeMatrix (SomeMatrix m) f = withProj2 m $ \(SomeMatrix' m') -> withProj2 m' f

-- 隠したMatrixのshowを呼び出す
instance Show SomeMatrix where
  show sm = withSomeMatrix sm show

-- 形だけ読めればいいので"2 3"と空白入れて1行で形を読むだけの定義
readMatrix :: IO SomeMatrix
readMatrix = do
  line <- getLine
  let [a, b] = map read (words line) :: [DemoteRep Nat]
  withSomeSing a $ \sa ->
    withSomeSing b $ \sb ->
      return $ SomeMatrix (Sigma sa (SomeMatrix' (Sigma sb (Matrix sa sb))))

toProxy :: Sing a -> Proxy a
toProxy _ = Proxy

maybeProd :: SomeMatrix -> SomeMatrix -> Maybe SomeMatrix
maybeProd sm1 sm2 =
  withSomeMatrix sm1 $ \m1 ->
  withSomeMatrix sm2 $ \m2 ->
    case (m1, m2) of
      (Matrix r1 c1, Matrix r2 c2) ->
        case withKnownNat c1 $ withKnownNat r2 $ sameNat (toProxy c1) (toProxy r2) of
          Nothing   -> Nothing
          Just Refl -> Just (SomeMatrix (Sigma r1 (SomeMatrix' (Sigma c2 (prod m1 m2)))))

main :: IO ()
main = do
  m1 <- readMatrix
  m2 <- readMatrix
  case maybeProd m1 m2 of
    Nothing -> error "Invalid"
    Just m3 -> print m3

