{-# OPTIONS_GHC -Wall #-}

module Logic.Coalition.Evaluation.Benchmark.Pilot where

import qualified Data.Set as Set
import General (isProvableZ)
import Logic.Coalition.Amir_CL
  ( clWithAgents
  , isvalidformula
  )
import Logic.Coalition.Evaluation.Benchmark.BenchmarkFamilies
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)

pilot :: [FamilyBenchmark]
pilot =
     [ superadditivity n       | n <- [2, 3, 4, 5, 6] ]
  ++ [ superadditivityprime n  | n <- [2, 3, 4, 5, 6] ]
  ++ [ outcomeMonotonicity n   | n <- [2, 3, 4, 5, 6] ]
  ++ [ coalitionMonotonicity n | n <- [2, 3, 4, 5, 6] ]
  ++ [ nestingBox n            | n <- [1, 2, 3, 4, 5] ]
  ++ [ chainCL n               | n <- [2, 3, 4, 5, 6] ]
  ++ [ nestedChainCL n         | n <- [2, 3, 4, 5, 6] ]


file :: FilePath
file =
  "evaluation/raw/pilot-results.txt"

save :: String -> IO ()
save message = do
  putStrLn message
  appendFile file (message ++ "\n")


runCasesOf :: FamilyBenchmark -> IO Bool
runCasesOf test = do
  let name = benchmarkFamily test
      n = benchmarkN test
      agentUniverse = benchmarkAgentUniverse test
      formula = benchmarkFormula test
      expected = expectedProvable test
      valid = isvalidformula agentUniverse formula

  if valid
    then do
      let result =
            isProvableZ
              (clWithAgents agentUniverse)
              formula

          agreement =
            result == expected

      save
        ( name
          ++ " n=" ++ show n
          ++ " agents=" ++ show (Set.size agentUniverse)
          ++ " valid=True"
          ++ " expected=" ++ show expected
          ++ " result=" ++ show result
          ++ " agreement=" ++ show agreement
        )

      pure agreement

    else do
      save
        ( name
          ++ " n=" ++ show n
          ++ " valid=False"
        )

      pure False


main :: IO ()
main = do
  createDirectoryIfMissing True "evaluation/raw"


  writeFile file ""


  results <- mapM runCasesOf pilot


  if and results
    then
      save "PILOT PASSED"
    else do
      save "PILOT FAILED"
      exitFailure
