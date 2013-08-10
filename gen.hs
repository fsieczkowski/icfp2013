module Gen where
  
  import Ast
  import List

  genexps :: [Var] -> Integer -> ([UnOp], [BinOp], Bool, Bool) -> [Exp]
  genexps vars depth ops =
    map (\ (x, y) -> x) . filter (\(x, y) -> y == depth) $ genAux vars depth ops


  genAux vars 1 ops = (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars
  genAux vars depth (uops, binops, canIf, canFold) =
    if depth <= 0 then [] else
      sub ++
      [(UOp uop e, depth) | uop <- uops, (e, sz) <- sub, sz + 1 == depth] ++
      bins sub ++
      (if canIf then [(IfZ e1 e2 e3, depth) | (e1, sz1) <- cutAt (depth - 3) sub, 
                      (e2, sz2) <- cutAt (depth - sz1 - 2) sub,
                      (e3, sz3) <- cutAt (depth - sz1 - sz2 - 1) sub, sz1 + sz2 + sz3 + 1 == depth] else []) ++
      (if canFold then [(Fold e1 e2 "y" "z" e3, depth) | (e1, sz1) <- subNF, (e2, sz2) <- cutAt (depth - sz1 - 3) subNF, 
                        (e3, sz3) <- cutAt (depth - sz1 - sz2 - 2) subNF, sz1 + sz2 + sz3 + 2 == depth] else [])
    where cutAt _ [] = []
          cutAt n ((e, m) : xs) = if n < m then [] else (e, m) : cutAt n xs
          bins [] = []
          bins ((e1, sz1) : es) = [(BOp bop e1 e2, depth) | bop <- binops, (e2, sz2) <- cutAt (depth - 2) es,
                                   sz1 + sz2 + 1 == depth] ++ bins es
          sub = filter (\(e, _) -> sensibleNEx (uops, binops, canIf) e) $ 
                genAux vars (depth - 1) (uops, binops, canIf, canFold)
          subNF = filter (\(e, _) -> sensibleNEx (uops, binops, canIf) e) $
                  genAux ("y" : "z" : vars) (depth - 3) (uops, binops, canIf, False)

  genProgs depth ops =
    map (Lam "x") . filter (sensible ops) $ genexps ["x"] (depth - 1) ops

  genProgsTFold depth uops binops canIf =
    map (\e -> Lam "x" (Fold (Var "x") Zero "x" "y" e)) . filter (sensible (uops, binops, canIf, False)) $ 
      genexps ["x", "y"] (depth - 5) (uops, binops, canIf, False)

  sensible (ops @ (uops, _, _, _)) exp =
    isNothing (allUsed ops exp) && notTFold exp &&
    ordshifts exp && rsSimpl uops exp && leftLin exp
    where notTFold (Fold _ _ _ _ _) = False
          notTFold _ = True

  sensibleNEx (ops @ (uops, _, _)) exp =
    ordshifts exp && rsSimpl uops exp && leftLin exp

  allUsed ([], [], False, False) exp  = Nothing
  allUsed ops Zero = Just ops
  allUsed ops One  = Just ops
  allUsed ops (Var _) = Just ops
  allUsed (us, bs, reqIf, reqFold) (UOp uo e) = allUsed (filter ((/=) uo) us, bs, reqIf, reqFold) e
  allUsed (us, bs, reqIf, reqFold) (BOp bo e1 e2) = 
    allUsed (us, filter ((/=) bo) bs, reqIf, reqFold) e1 >>= \ops -> allUsed ops e2
  allUsed (us, bs, reqIf, reqFold) (IfZ e1 e2 e3) = 
    allUsed (us, bs, False, reqFold) e1 >>= flip allUsed e2 >>= flip allUsed e3
  allUsed (us, bs, reqIf, reqFold) (Fold e1 e2 x y e3) = 
    allUsed (us, bs, reqIf, False) e1 >>= flip allUsed e2 >>= flip allUsed e3
                                    
  isNothing Nothing  = True
  isNothing (Just _) = False
  
  ordshifts (UOp Shr4  (UOp Shr1 _)) = False
  ordshifts (UOp Shr16 (UOp Shr1 _)) = False
  ordshifts (UOp Shr16 (UOp Shr4 _)) = False
  ordshifts _ = True
                  
  rsSimpl us (UOp Shr1 Zero) = True
  rsSimpl us (UOp Shr1 One) = False
  rsSimpl us (UOp Shr4 Zero) = if (elem Shr1 us) then False else True
  rsSimpl us (UOp Shr4 One) = False
  rsSimpl us (UOp Shr16 Zero) = if (any (\u -> u == Shr1 || u == Shr4) us) then False else True
  rsSimpl us (UOp Shr16 One) = False
  rsSimpl _ _ = True
  
  leftLin (BOp b1 e1 (e2 @ (BOp b2 _ _ ))) = if b1 == b2 then False else True
  leftLin _ = True