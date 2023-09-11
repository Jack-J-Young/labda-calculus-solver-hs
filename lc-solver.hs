import Data.List

data PureLC = Lambda String PureLC | App PureLC PureLC | Var String
   deriving Show
data SugaredLC = LambdaS [String] SugaredLC | AppS [SugaredLC] | VarS String
   deriving Show

endBracket :: String -> Int -> [String]
endBracket ('(':rest) 0 = endBracket rest 1
endBracket (')':rest) 1 = ["", rest]
endBracket string 0 = ["", string]
endBracket ('(':tail) n = [a, b]
   where
      t = endBracket tail (n + 1)
      a = '(':head t
      b = last t
endBracket (')':tail) n = [a, b]
   where
      t = endBracket tail (n - 1)
      a = ')':head t
      b = last t
endBracket (h:tail) n = [a, b]
   where
      t = endBracket tail n
      a = h:head t
      b = last t

headBody :: String -> [String]
headBody ('.':body) = ["", body]
headBody (c:body) = [h, b]
   where
      result = headBody body
      h = c:head result
      b = last result

stringToSugaredLC :: String -> SugaredLC
stringToSugaredLC ('(':t) = o
   where
      parts = endBracket ('(':t) 0
      sh = stringToSugaredLC (head parts)
      st = if last parts == ""
         then
            VarS ""
         else
            stringToSugaredLC (last parts)
      o = if last parts == ""
         then
            sh
         else
            case st of
               AppS list -> AppS (sh:list)
               el -> AppS [sh, el]
stringToSugaredLC ('^':rest) = LambdaS vars body
   where
      result = headBody rest
      vars = map (: []) (head result)
      body = stringToSugaredLC (head (tail result))
stringToSugaredLC (h:'(':t) = AppS [VarS (h:""), stringToSugaredLC ('(':t)]
stringToSugaredLC (h:ht:t) = x
   where
      rest = stringToSugaredLC (ht:t)
      x = case rest of
         AppS list -> AppS (VarS (h:""):list)
         r -> AppS [VarS (h:""), r]
stringToSugaredLC (h:"") = VarS (h:"")
stringToSugaredLC s = VarS s

pureLCtoString :: PureLC -> String
pureLCtoString (Lambda v body) = "(^" ++ v ++ "." ++ pureLCtoString body ++ ")"
pureLCtoString (App lc1 lc2) = "(" ++ pureLCtoString lc1 ++ " " ++ pureLCtoString lc2 ++ ")"
pureLCtoString (Var v) = v

lcEq :: PureLC -> PureLC -> Bool
lcEq (Lambda v1 b1) (Lambda v2 b2) = v1 == v2 && lcEq b1 b2
lcEq (App lc11 lc12) (App lc21 lc22) = lcEq lc11 lc21 && lcEq lc12 lc22
lcEq (Var v1) (Var v2) = v1 == v2
lcEq _ _ = False

replaceLC :: PureLC -> String -> PureLC -> PureLC
replaceLC (Lambda fv body) v r = Lambda fv (if fv == v then body else replaceLC body v r)
replaceLC (App lc1 lc2) v r = App (replaceLC lc1 v r) (replaceLC lc2 v r)
replaceLC (Var vv) v r = if vv == v then r else Var vv

betaReduce :: PureLC -> PureLC
betaReduce (Lambda var body) = Lambda var (betaReduce body)
betaReduce (App (Lambda var body) a) = replaceLC body var a
--betaReduce (App (Lambda var body) (Lambda var1 body1)) = replaceLC body var (Lambda var1 body1)
betaReduce (App lc1 lc2) = App (betaReduce lc1) (betaReduce lc2)
betaReduce (Var v) = Var v

toNormal :: PureLC -> PureLC
toNormal lc = x
   where
      br = betaReduce lc
      x = if lcEq br lc then lc else toNormal br 

desugar :: SugaredLC -> PureLC
desugar (VarS a) = Var a
desugar (AppS [a, b]) = App (desugar a) (desugar b)
desugar (AppS listLc) = App (desugar (AppS (init listLc))) (desugar (last listLc))
desugar (LambdaS [] body) = desugar body
desugar (LambdaS (h:t) body) = Lambda h (desugar (LambdaS t body))

fv :: PureLC -> [String]
fv (Lambda s lc) = filter (/= s) (fv lc)
fv (App lc1 lc2) = nub (fv lc1 ++ fv lc2)
fv (Var s) = [s]

main = do
   print(pureLCtoString (toNormal (desugar (stringToSugaredLC "(^tpads.(^mg.(^hij.(thh)m(s(tii)(tjj)))(m(gm(t(tgg)g)))(tgg)(m(tgg)))(^nfx.a(n(^k.p((dk)(f(ak))(ak))(^xy.x))(px(^xy.y))))(^fx.f(fx)))(^abfx.a(bf)x)(^abf.fab)(^p.p(^ab.a))(^p.p(^ab.b))(^abfx.af(bfx))"))))