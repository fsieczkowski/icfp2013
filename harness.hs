module Harness where

  import Ast
  import Gen
  import Test
  
  churn progs =
    do
      print $ "Number of programs generated: " ++ show (length progs) ++ "\n"
      (tests, buckets) <- test progs
      print $ "Number of tests generated: " ++ show (length tests) ++ "\n"
      print $ "Maximal number of programs in a bucket: " ++
        show (maximum (map (\(rs, ps) -> length ps) buckets)) ++ "\n"
      return (tests, buckets)

  -- generate all the "simple" programs (w/o folds), get the tests and the result-coded buckets of programs
  -- returns a pair (tests, buckets); use the buckets later w/ findProgs function
  runSimple depth uops bops =
    let progs = genProgs depth uops bops
    in churn progs

  -- generate all the programs with the 'tfold' operation, get the tests and the result-coded buckets of programs
  -- returns a pair (tests, buckets); use the buckets later w/ findProgs function
  runTFold depth uops bops =
    let progs = genProgsTFold depth uops bops
    in churn progs

  -- find the programs that match the list of results
  findProgs results buckets =
    lookup results buckets