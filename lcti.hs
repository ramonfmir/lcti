import Data.Bool
import Data.List
import Data.Maybe

-------------------------------------------------------------------------------
data Term = Var Char | Abs Char Term | App Term Term deriving (Eq, Show)
data Type = Phi Int | Arrow Type Type deriving (Eq, Show)

type Ctx = [(Term, Type)]
type Sub = Type -> Type

-------------------------------------------------------------------------------
pp :: Term -> Maybe Type
pp tm = tp
  where
    (_, _, tp) = pp' tm 0

pp' :: Term -> Int -> (Int, Ctx, Maybe Type)
pp' tm@(Var x) i
  = let tp = Phi (i + 1) in (i + 1, [(tm, tp)], Just tp)
pp' (Abs x tm) i
  = let (j, ctx, tp') = pp' tm i in case tp' of
      Nothing -> (0, [], Nothing)
      Just tp -> case lookup (Var x) ctx of
                   Nothing -> (j + 1, ctx, Just (Arrow (Phi (j + 1)) tp))
                   Just e  -> (j, ctx \\ [((Var x), e)], Just (Arrow e tp))
pp' (App n m) i
  = case tp1 of
    Nothing   -> (0, [], Nothing)
    Just tp1' -> case tp2 of
                   Nothing   -> (0, [], Nothing)
                   Just tp2' -> let s1 = unify tp1' (Arrow tp2' (Phi (k + 1))) in
                                case s1 of
                                  Nothing  -> (0, [], Nothing)
                                  Just s1' -> let s2 = unifyctxs (subs s1' ctx1) (subs s1' ctx2) in
                                              case s2 of
                                                Nothing  -> (0, [], Nothing)
                                                Just s2' -> let s = s2' . s1' in (k + 1, subs s (ctx1 ++ ctx2), Just (s (Phi (k + 1))))
  where
    (j, ctx1, tp1) = pp' n i
    (k, ctx2, tp2) = pp' m j

subs :: Sub -> Ctx -> Ctx
subs s = map (\(tm, tp) -> (tm, s tp))

unify :: Type -> Type -> Maybe Sub
unify tp@(Phi _) tp'
  | appears tp tp' = Nothing
  | otherwise      = Just (\x -> if x == tp then tp' else x)
  where
    appears (Phi x) (Phi y)     = x == y
    appears tp1 (Arrow tp2 tp3) = (appears tp1 tp2) || (appears tp1 tp3)
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

unifyctxs :: Ctx -> Ctx -> Maybe Sub
unifyctxs [] _
  = Just (\x -> x)
unifyctxs ((tm, tp):ctx) ctx'
  = case lookup tm ctx' of
      Nothing  -> unifyctxs ctx ctx'
      Just tp' -> let s = unify tp tp' in case s of
                    Nothing -> Nothing
                    Just s' -> case s'' of
                                 Nothing   -> Nothing
                                 Just s''' -> Just (s''' . s')
                      where
                        s'' = unifyctxs (subs s' ctx) (subs s' ctx'')
                        ctx'' = ctx' \\ [(tm, tp)]

-------------------------------------------------------------------------------

s :: Type -> Type
s (Phi 1)
  = (Phi 2)
s x = x

context :: Ctx
context
  = [(Var 'x', Phi 1), (Var 'y', Phi 3)]
