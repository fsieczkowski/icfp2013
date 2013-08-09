module Gen where
  
  import Ast

  genexps :: [Var] -> Integer -> [UnOp] -> [BinOp] -> [Exp]
  genexps vars depth uops binops =
    map (\ (x, y) -> x) . filter (\(x, y) -> y == depth) $ genAux vars depth uops binops


  genAux vars 1 uops binops = (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars
  genAux vars depth uops binops =
    if depth <= 0 then [] else
    (Zero, 1) : (One, 1) : map (\x -> (Var x, 1)) vars ++
    concatMap (\(exp, sz) -> map (\uop -> (UOp uop exp, sz + 1)) uops) sub ++
    bops sub
    where bops [] = []
          bops (xs @ ((e, sz) : xs')) =
            [(BOp bop e e', sz + sz' + 1) | bop <- binops, (e', sz') <- xs, sz + sz' < depth] ++
              bops xs'
          sub = (genAux vars (depth - 1) uops binops)

  genProgs depth uops binops = map (Lam "x") $ genexps ["x"] (depth - 1) uops binops

  genProgsTFold depth uops binops =
    map (\e -> Lam "x" (Fold (Var "x") Zero "x" "y" e)) $ genexps ["x", "y"] (depth - 5) uops binops
