{-# OPTIONS_GHC -Wall #-}
module Logic.Coalition.Evaluation.PostHoc.Dblrun2nd where

import GHC.Clock (getMonotonicTimeNSec)
import General (isProvableZ)
import Logic.Coalition.Amir_CL
  ( clWithAgents
  , isvalidformula
  )
import Logic.Coalition.Evaluation.Benchmark.BenchmarkFamilies
import Logic.Coalition.Evaluation.Benchmark.BenchmarkProperties
import System.Timeout (timeout)
import System.Directory (createDirectoryIfMissing)
import Control.Exception
  ( SomeException
  , evaluate
  , try
  )
-- System.Timeout requires microseconds.
timeoutSeconds :: Int
timeoutSeconds =
  100

timeoutLimit :: Int
timeoutLimit =
  timeoutSeconds * 1000000

csvFile :: FilePath
csvFile =
  "evaluation/PostHoc/double-runtime-2nd-2026-07-18.csv"

csvHeader :: String
csvHeader =
  "family,n,run,pieces,boxes,deepest_box,agents,"
  ++ "agents-in-coalitions,expected,valid,"
  ++ "genz,agreement,timeout,runtime,status"

csv :: String -> IO ()
csv row =
  appendFile csvFile (row ++ "\n")

-- Run and measure one benchmark case.
oneCase :: Int -> FamilyBenchmark -> IO Bool
oneCase run familyBenchmark = do
  let formula =
        benchmarkFormula familyBenchmark

      agents =
        benchmarkAgentUniverse familyBenchmark

      isexpected =
        expectedProvable familyBenchmark

      isvalid =
        isvalidformula agents formula

      agentsInCoalitions =
        allAgentsInCoalition formula

      row =
        benchmarkFamily familyBenchmark
          ++ "," ++ show (benchmarkN familyBenchmark)
          ++ "," ++ show run
          ++ "," ++ show (numberofpieces formula)
          ++ "," ++ show (numberofboxes formula)
          ++ "," ++ show (numberofdeepestBox formula)
          ++ "," ++ show (numberOfAgents agents)
          ++ "," ++ show agentsInCoalitions
          ++ "," ++ show isexpected
          ++ "," ++ show isvalid

  putStrLn ("Preparing: " ++ row)

  if isvalid
    then do
      let cl =
            clWithAgents agents

          proof =
            isProvableZ cl formula

      startTime <-
        getMonotonicTimeNSec

      result <-
        ( try (timeout timeoutLimit (evaluate proof))
            :: IO (Either SomeException (Maybe Bool))
        )

      endTime <-
        getMonotonicTimeNSec

      let runtime =
            fromIntegral (endTime - startTime)
              / 1000000.0 :: Double

      case result of
        Left runerror -> do
          let genz =
                ""

              agreement =
                ""

              timeoutResult =
                show timeoutSeconds

              runtimeResult =
                show runtime

              status =
                "RuntimeError"

          putStrLn
            ("RuntimeError: " ++ show runerror)

          csv
            ( row
              ++ "," ++ genz
              ++ "," ++ agreement
              ++ "," ++ timeoutResult
              ++ "," ++ runtimeResult
              ++ "," ++ status
            )

          pure False

        Right Nothing -> do
          let genz =
                ""

              agreement =
                ""

              timeoutResult =
                show timeoutSeconds

              runtimeResult =
                show runtime

              status =
                "Timeout"

          putStrLn
            ( "Status: Timeout at "
              ++ show timeoutSeconds
              ++ " seconds"
            )

          csv
            ( row
              ++ "," ++ genz
              ++ "," ++ agreement
              ++ "," ++ timeoutResult
              ++ "," ++ runtimeResult
              ++ "," ++ status
            )

          pure False

        Right (Just genzAnswer) -> do
          let agree =
                genzAnswer == isexpected

              genz =
                show genzAnswer

              agreement =
                show agree

              timeoutResult =
                show timeoutSeconds

              runtimeResult =
                show runtime

              status =
                if agree
                  then "Completed"
                  else "LogicProblem"

          putStrLn
            ( "Status: " ++ status
              ++ "\nGenZ answer: " ++ genz
              ++ "\nAgreement: " ++ agreement
              ++ "\nRuntime: " ++ runtimeResult
            )

          csv
            ( row
              ++ "," ++ genz
              ++ "," ++ agreement
              ++ "," ++ timeoutResult
              ++ "," ++ runtimeResult
              ++ "," ++ status
            )

          pure agree

    else do
      let genz =
            ""

          agreement =
            ""

          timeoutResult =
            show timeoutSeconds

          runtimeResult =
            ""

          status =
            "InvalidFormula"

      putStrLn
        "Status: InvalidFormula"

      csv
        ( row
          ++ "," ++ genz
          ++ "," ++ agreement
          ++ "," ++ timeoutResult
          ++ "," ++ runtimeResult
          ++ "," ++ status
        )

      pure False


workingrun :: FamilyBenchmark -> IO Bool
workingrun familyBenchmark = do
  putStrLn "---------- Run 1 ----------"

  run1 <- oneCase 1 familyBenchmark

  if run1
    then do
      putStrLn "---------- Run 2 ----------"

      run2 <- oneCase 2 familyBenchmark

      if run2
        then do
          putStrLn "---------- Run 3 ----------"

          run3 <- oneCase 3 familyBenchmark

          pure run3

        else
          pure False

    else
      pure False

familyRunning :: [FamilyBenchmark] -> IO ()
familyRunning [] =
  pure ()

familyRunning (familyBenchmark : remaining) = do
  allWorked <-
    workingrun familyBenchmark

  if allWorked
    then
      familyRunning remaining
    else
      putStrLn
        "family stopped"


main :: IO ()
main = do
  createDirectoryIfMissing
    True
    "evaluation/PostHoc"

  writeFile csvFile (csvHeader ++ "\n")

  let dbl =
        [ 40
        , 80
        , 160
        , 320
        , 640
        , 1280
        , 2560
        , 5120
        ]


  putStrLn "coalition monotonicity"
  familyRunning
    [ coalitionMonotonicity n
    | n <- dbl
    ]

  putStrLn " nesting boxes"
  familyRunning
    [ nestingBox n
    | n <- dbl
    ]

  putStrLn " outcome monotonicity "
  familyRunning
    [ outcomeMonotonicity n
    | n <- dbl
    ]


  putStrLn " doubling experiment finished"