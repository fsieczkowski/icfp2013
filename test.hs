module Test where
  import Ast
  import Eval
  import Data.Word
  import System.Random
  import System.IO
  import System.IO.Unsafe

  instance Random Word64 where
    randomR (lo, hi) g = (fromInteger n, g')
      where (n, g') = (randomR (toInteger lo, toInteger hi) g)
    random g = randomR (minBound, maxBound) g
  
  test :: [Prog] -> IO ([Word64], [([Word64], [Prog])])
  test progs = testAux 1024 ([], [([], progs)])

  testAux :: Integer -> ([Word64], [([Word64], [Prog])]) -> IO ([Word64], [([Word64], [Prog])])
  testAux runs (tests, buckets) =
    if runs == 0 || length tests == 256 || all (\(rs, ps) -> length ps == 1) buckets 
    then return (tests, buckets)
    else do
      n <- randomIO
      let (buckets', split) = foldr (splitBucket n) ([], False) buckets
      if split then testAux (runs - 1) (n : tests, buckets')
        else testAux (runs - 1) (tests, buckets)
    where
      splitBucket n (ress, progs) (bs, flag) =
        let insert [] n p = [(n:ress, [p])]
            insert ((m:ress, ps) : bs) n p =
              if n == m then ((m : ress, p : ps) : bs)
              else (m : ress, ps) : insert bs n p
            aux bucks [] = bucks
            aux bucks (p : progs) =
              aux (insert bucks (eval p n) p) progs
            nbucks = aux [] progs
        in (nbucks, if length nbucks == 1 then flag else True)