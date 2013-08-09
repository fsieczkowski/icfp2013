module Gen where
  
  import Ast

  genexps :: [Var] -> Integer -> [UnOp] -> [BinOp] -> Bool -> [Exp]
  genexps vars depth uops binops canIf =
    map (\ (x, y) -> x) . filter (\(x, y) -> y == depth) $ genAux vars depth uops binops canIf


  genAux vars 1 uops binops canIf = (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars
  genAux vars depth uops binops canIf =
    if depth <= 0 then [] else
    (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars ++
    concatMap (\(exp, sz) -> map (\uop -> (UOp uop exp, sz + 1)) uops) sub ++
    bops sub ++ 
    [(IfZ e1 e2 e3, sz1 + sz2 + sz3 + 1) | (e1, sz1) <- sub, (e2, sz2) <- sub, 
       (e3, sz3) <- sub, sz1 + sz2 + sz3 < depth]
    where bops [] = []
          bops (xs @ ((e, sz) : xs')) =
            [(BOp bop e e', sz + sz' + 1) | bop <- binops, (e', sz') <- xs, sz + sz' < depth] ++
              bops xs'
          sub = genAux vars (depth - 1) uops binops canIf

  genProgs depth uops binops canIf =
    map (Lam "x") . filter (sensible uops binops) $ genexps ["x"] (depth - 1) uops binops canIf

  genProgsTFold depth uops binops canIf =
    map (\e -> Lam "x" (Fold (Var "x") Zero "x" "y" e)) . filter (sensible uops binops) $ 
      genexps ["x", "y"] (depth - 5) uops binops canIf

  sensible uops binops exp =
    isNothing (allUsed (uops, binops) exp) &&
    (if rshiftcnt uops > 1 then ordshifts 1 exp else True) &&
    rsSimpl exp

    where allUsed ([], []) exp  = Nothing
          allUsed ops Zero = Just ops
          allUsed ops One  = Just ops
          allUsed ops (Var _) = Just ops
          allUsed (us, bs) (UOp uo e) = allUsed (filter ((/=) uo) us, bs) e
          allUsed (us, bs) (BOp bo e1 e2) = allUsed (us, filter ((/=) bo) bs) e1 >>= \ops -> allUsed ops e2
          allUsed ops (IfZ e1 e2 e3) = allUsed ops e1 >>= flip allUsed e2 >>= flip allUsed e3
          allUsed ops (Fold e1 e2 x y e3) = allUsed ops e1 >>= flip allUsed e2 >>= flip allUsed e3
          
          isNothing Nothing  = True
          isNothing (Just _) = False

          rshiftcnt [] = 0
          rshiftcnt (Shr1 : xs) = 1 + rshiftcnt xs
          rshiftcnt (Shr4 : xs) = 1 + rshiftcnt xs
          rshiftcnt (Shr16 : xs) = 1 + rshiftcnt xs
          rshiftcnt (_ : xs) = rshiftcnt xs
          
          ordshifts n (UOp Shr1 e)  = if n <= 1  then ordshifts 1  e else False
          ordshifts n (UOp Shr4 e)  = if n <= 4  then ordshifts 4  e else False
          ordshifts _ (UOp Shr16 e) = ordshifts 16 e
          ordshifts _ (UOp _ e) = ordshifts 1 e
          ordshifts _ (BOp _ e1 e2) = ordshifts 1 e1 && ordshifts 1 e2 
          ordshifts _ (IfZ e1 e2 e3) = ordshifts 1 e1 && ordshifts 1 e2 && ordshifts 1 e3
          ordshifts _ (Fold e1 e2 _ _ e3) = ordshifts 1 e1 && ordshifts 1 e2 && ordshifts 1 e3
          ordshifts _ _ = True

          rsNot1 Shr4  = True
          rsNot1 Shr16 = True
          rsNot1 _ = False
          
          constant Zero = True
          constant One  = True
          constant _ = False

          rsSimpl (UOp Shr1 Zero) = True
          rsSimpl (UOp Shr1 One) = False
          rsSimpl (UOp op exp) = if rsNot1 op && constant exp then False else rsSimpl exp
          rsSimpl (BOp _ e1 e2) = rsSimpl e1 && rsSimpl e2
          rsSimpl (IfZ e1 e2 e3) = rsSimpl e1 && rsSimpl e2 && rsSimpl e3
          rsSimpl (Fold e1 e2 _ _ e3) = rsSimpl e1 && rsSimpl e2 && rsSimpl e3
          rsSimpl _ = True
