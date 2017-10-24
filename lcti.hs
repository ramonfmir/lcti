import Data.Maybe

-------------------------------------------------------------------------------
data Term = Var Char | Abs Char Term | App Term Term deriving (Eq, Show)
data Type = Phi Int | Arrow Type Type deriving (Eq, Show)

type Ctx = [(Term, Type)]
type Sub = Type -> Type

-------------------------------------------------------------------------------
pp :: Term -> Maybe Type
pp tm = pp' tm [] []

pp' :: Term -> Ctx -> [Int] -> Maybe Type
pp' (Var x) c types
  = Nothing

subs :: Maybe Sub -> Ctx -> Maybe Ctx
subs Nothing _
  = Nothing
subs (Just s) c
  = Just (map (\(tm, tp) -> (tm, s tp)) c)

unify :: Type -> Type -> Maybe Sub
unify tp@(Phi _) tp'@(Phi _)
  = Just (\x -> if x == tp then tp' else x)
unify tp@(Phi x) tp'
  = Nothing
unify tp tp'@(Phi _)
  = unify tp' tp
unify (Arrow tp1 tp2) (Arrow tp1' tp2')
  = compose ms' ms
  where
    ms  = unify tp1 tp1'
    ms' = unify' ms tp2 tp2'
      where
        unify' (Just s) tp tp' = unify (s tp) (s tp')
        unify' _ _ _           = Nothing
    compose (Just s) (Just s') = Just (s . s')
    compose _ _                = Nothing



-------------------------------------------------------------------------------

s :: Type -> Type
s (Phi 1)
  = (Phi 2)
s x = x

context :: Ctx
context
  = [(Var 'x', Phi 1), (Var 'y', Phi 3)]
