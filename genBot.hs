module GenBot where

  import Ast
  import Data.Bits
  import Data.Word
  import Data.List
  
  tests = [0, -1] ++ [shiftL 1 n | n <- [0 .. 63]] ++ [rotateL (-2) n | n <- [0 .. 63]]
  
  res0 = take 130 $ repeat 0 :: [Word64]
  res1 = take 130 $ repeat 1
  
  evUOp Not   = complement
  evUOp Shl1  = \x -> shiftL x 1
  evUOp Shr1  = \x -> shiftR x 1
  evUOp Shr4  = \x -> shiftR x 4
  evUOp Shr16 = \x -> shiftR x 16
  
  evBOp Plus = (+)
  evBOp And  = (.&.)
  evBOp Or   = (.|.)
  evBOp Xor  = xor

  genBot 1  ops = [[(res0, [Zero]), (tests, [Var "x"]), (res1, [One])]]
  genBot sz ops =
    if sz <= 0 then []
    else
      genNext sz ops subs
    where subs = genBot (sz - 1) ops

  genNext sz (ops @(uops, binops, canIf, canFold)) subs =
    subs ++ [compactify (sortBy (\(rs1, ps1) (rs2, ps2) -> compare rs1 rs2) (unis ++ bins 1 subs ++ ifs ++ folds))]
    where compactify [] = []
          compactify (p : ps) = squeeze p ps
          squeeze (rss, ps) [] = [(rss, ps)]
          squeeze (rss, ps) ((rss', ps') : rst) =
            if (rss == rss') then squeeze (rss, ps ++ ps') rst else (rss, ps) : squeeze (rss', ps') rst

          unis = [(map (evUOp uo) ress, map (UOp uo) ps) | uo <- uops, (ress, ps) <- last subs]
          bins _ [] = []
          bins n ([] : xs) = if 2 * n + 1 >= sz then [] else bins (n + 1) xs
          bins n (ys @ (((rs1, ps1) : xs) : ys')) =
            bins n (xs : ys') ++
            [(zipWith (evBOp bo) rs1 rs2, zipWith (BOp bo) ps1 ps2) | bo <- binops, sz >= 2* n + 1, (rs2, ps2) <- ys !! (sz - 2 * n - 1)]
          ifs  = if canIf && sz >= 4 then
                   [(zipWith3 (\v1 v2 v3 -> if v1 == 0 then v2 else v3) rs1 rs2 rs3, zipWith3 IfZ ps1 ps2 ps3) |
                    n1 <- [0 .. sz-4], n2 <- [0..sz - 4 - n1], (rs1, ps1) <- (subs !! n1),
                    (rs2, ps2) <- (subs !! n2), (rs3, ps3) <- (subs !! (sz - n1 - n2 - 4))]
                 else []
          folds = [] -- not implemented!

