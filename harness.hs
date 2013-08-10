module Harness where

  import Ast
  import Gen
  import Test
  import Numeric
  import Data.Word
  import System.Process

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
  printTests = print . map (\t -> "0x" ++ showHex t "")

  -- find the programs that match the list of results
  findProgs results buckets =
    lookup (map read results) buckets

  -- these do not work (malformed JSON, they say) and I don't get why.
  reval id tests = readProcess "curl" ["-XPOST", "http://icfp2013lf.herokuapp.com/evaluess?auth=0229KtQKyHAgd8LaD0JPubHAC9InNBjCPTxnhVQBvpsH1H", "-d '{\"id\":\"" ++ id ++ "\", \"arguments\":" ++ show (map (\t -> "0x" ++ showHex t "") tests) ++ "}'"] ""
  
  rguess id prog = readProcess "curl" ["-XPOST", "http://icfp2013lf.herokuapp.com/guess?auth=0229KtQKyHAgd8LaD0JPubHAC9InNBjCPTxnhVQBvpsH1H", "-d '{\"id\":\"" ++ id ++ "\", \"arguments\":\"" ++ show prog ++ "\"}'"] ""