module Harness where

  import Ast
  import Gen
  import Test
  import Numeric
  import Data.Word
  
  churn progs =
    do
      putStrLn $ "Number of programs generated: " ++ show (length progs)
      (tests, buckets) <- test progs
      putStrLn $ "Number of tests generated: " ++ show (length tests)
      putStrLn $ "Maximal number of programs in a bucket: " ++
        show (maximum (map (\(rs, ps) -> length ps) buckets))
      return (tests, buckets)

  -- generate all the "simple" programs (w/o folds), get the tests and the result-coded buckets of programs
  -- returns a pair (tests, buckets); use the buckets later w/ findProgs function
  runSimple depth uops bops canIf =
    let progs = genProgs depth uops bops canIf
    in churn progs

  -- generate all the programs with the 'tfold' operation, get the tests and the result-coded buckets of programs
  -- returns a pair (tests, buckets); use the buckets later w/ findProgs function
  runTFold depth uops bops canIf =
    let progs = genProgsTFold depth uops bops canIf
    in churn progs

  -- prints a list of words (e.g., tests) in hexadecimal. Also prints a trailing comma, cause I'm lazy
  printTests :: [Word64] -> IO ()
  printTests = mapM_ (putStrLn . flip showHex ", ")

  -- find the programs that match the list of results
  findProgs results buckets =
    lookup results buckets