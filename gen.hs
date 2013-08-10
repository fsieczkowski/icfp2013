module Gen where
  
  import Ast
  import List

  genexps :: [Var] -> Integer -> [UnOp] -> [BinOp] -> Bool -> [Exp]
  genexps vars depth uops binops canIf =
    map (\ (x, y) -> x) . filter (\(x, y) -> y == depth) $ genAux vars depth uops binops canIf


  genAux vars 1 uops binops canIf = (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars
  genAux vars depth uops binops canIf =
    if depth <= 0 then [] else
    (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars ++
    concatMap (\(exp, sz) -> map (\uop -> (UOp uop exp, sz + 1)) uops) sub ++
    [(BOp bop e1 e2, sz1 + sz2 + 1) | bop <- binops, (e1, sz1) <- cutAt (depth - 2) sub,
                                      (e2, sz2) <- cutAt (depth - sz1 - 1) sub] ++
    (if canIf then [(IfZ e1 e2 e3, sz1 + sz2 + sz3 + 1) | (e1, sz1) <- cutAt (depth - 3) sub, 
                    (e2, sz2) <- cutAt (depth - sz1 - 2) sub,
                    (e3, sz3) <- cutAt (depth - sz1 - sz2 - 1) sub] else [])
    where cutAt _ [] = []
          cutAt n ((e, m) : xs) = if n < m then [] else (e, m) : cutAt n xs
          sub = sortBy (\ (e1, s1) (e2, s2) -> compare s1 s2) . 
                filter (\(e, _) -> sensibleNEx (uops, binops, canIf) e) $ genAux vars (depth - 1) uops binops canIf

  genProgs depth uops binops canIf =
    map (Lam "x") . filter (sensible (uops, binops, canIf)) $ genexps ["x"] (depth - 1) uops binops canIf

  genProgsTFold depth uops binops canIf =
    map (\e -> Lam "x" (Fold (Var "x") Zero "x" "y" e)) . filter (sensible (uops, binops, canIf)) $ 
      genexps ["x", "y"] (depth - 5) uops binops canIf

  sensible (ops @ (uops, _, _)) exp =
    isNothing (allUsed ops exp) {-&&
    (if rshiftcnt uops > 1 then ordshifts 1 exp else True)
    && (if any ((==) Shr1) uops then rsSimpl exp else True)
    && leftLin exp-}

  sensibleNEx (ops @ (uops, _, _)) exp =
    (if rshiftcnt uops > 1 then ordshifts 1 exp else True)
    && (if any ((==) Shr1) uops then rsSimpl exp else True)
    && leftLin exp

  allUsed ([], [], False) exp  = Nothing
  allUsed ops Zero = Just ops
  allUsed ops One  = Just ops
  allUsed ops (Var _) = Just ops
  allUsed (us, bs, reqIf) (UOp uo e) = allUsed (filter ((/=) uo) us, bs, reqIf) e
  allUsed (us, bs, reqIf) (BOp bo e1 e2) = 
    allUsed (us, filter ((/=) bo) bs, reqIf) e1 >>= \ops -> allUsed ops e2
  allUsed (us, bs, reqIf) (IfZ e1 e2 e3) = allUsed (us, bs, False) e1 >>= flip allUsed e2 >>= flip allUsed e3
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
  
  leftLin (BOp b1 e1 (e2 @ (BOp b2 _ _ ))) = if b1 == b2 then False else (leftLin e1 && leftLin e2)
  leftLin (BOp b1 e1 e2) = leftLin e1 && leftLin e2
  leftLin (UOp op e) = leftLin e
  leftLin (IfZ e1 e2 e3) = leftLin e1 && leftLin e2 && leftLin e3
  leftLin (Fold e1 e2 _ _ e3) = leftLin e1 && leftLin e2 && leftLin e3
  leftLin _ = True