{-# OPTIONS_GHC -Wall #-}

module Logic.Coalition.Evaluation.Benchmark.BenchmarkFamilies where


import qualified Data.Set as Set
import Logic.Coalition.Amir_CL

-- Ag = {1,...,n}.
univAgent       :: Int -> AgentUniverse
univAgent  n =     Set.fromList [1 .. n]


-- Coalition {1,...,n}
coalitionGrowth :: Int -> Coalition
coalitionGrowth n = Set.fromList [1 .. n]

topCL :: CL
topCL = ImpCL BotCL BotCL

pAtom :: Int -> CL
pAtom number =  AtCL ("p" ++ show number)


conjunctionF :: [CL] -> CL
conjunctionF []       = topCL
conjunctionF [f]      = f
conjunctionF (f : xs) = ConCL f (conjunctionF xs)

innerFormula :: Int -> CL
innerFormula n  =
    conjunctionF
    [ pAtom number
    | number <- [1 .. n]
    ]


singleAgent  :: Int -> Coalition
singleAgent  i = Set.singleton i


data FamilyBenchmark = FamilyBenchmark
  { benchmarkFamily        :: String
  , benchmarkAgentUniverse :: AgentUniverse
  , benchmarkN             :: Int
  , expectedProvable       :: Bool
  , benchmarkFormula       :: CL
  }
  deriving (Show)




leftSuperadditivity :: Int -> CL
leftSuperadditivity  n =
    conjunctionF
    [ BoxCL (singleAgent i) (pAtom i)
    | i <- [1 .. n]
    ]

superadditivity :: Int -> FamilyBenchmark
superadditivity  n
 | n < 1 =
    error "n >= 1"
 | otherwise =
    FamilyBenchmark
        { benchmarkFamily = "superadditivity"
        , benchmarkAgentUniverse = univAgent n
        , benchmarkN = n
        , expectedProvable      = True
        , benchmarkFormula         =
                ImpCL
                 (leftSuperadditivity n)
                 (BoxCL (coalitionGrowth n) (innerFormula n))
        }



missingLastAgent   :: Int -> Coalition
missingLastAgent n = Set.fromList [1 .. n - 1]


superadditivityprime :: Int  -> FamilyBenchmark
superadditivityprime  n
   | n < 2 =
      error "n >= 2"
   | otherwise =
      FamilyBenchmark
        { benchmarkFamily = "superadditivityprime"
        , benchmarkAgentUniverse = univAgent n
        , benchmarkN = n
        , expectedProvable      = False
        , benchmarkFormula         =
            ImpCL
            (leftSuperadditivity n)
            (BoxCL (missingLastAgent n) (innerFormula n))
        }


outcomeMonotonicity :: Int -> FamilyBenchmark
--  controlled innerFormula growth
outcomeMonotonicity  n
  | n < 2 =
      error "n >= 2"
  | otherwise =
    FamilyBenchmark {
        benchmarkFamily = "outcomeMonotonicity"
    ,   benchmarkAgentUniverse = univAgent 2
    ,   benchmarkN = n
    ,   expectedProvable      = True
    ,   benchmarkFormula         =
        ImpCL
              (BoxCL (singleAgent 1) (innerFormula n))
              (BoxCL (singleAgent 1) (innerFormula 1))
        }


coalitionMonotonicity:: Int -> FamilyBenchmark
-- controlled coalition growth
coalitionMonotonicity  n
    | n < 2 =
        error "n>=2"
    | otherwise =
        FamilyBenchmark
         {
              benchmarkFamily   = "coalitionMonotonicity"
            , benchmarkAgentUniverse   = univAgent n
            , benchmarkN   =  n
            , expectedProvable        = True
            , benchmarkFormula           =
                                ImpCL
                                    (BoxCL (singleAgent 1) (pAtom 1))
                                    (BoxCL (coalitionGrowth n) (pAtom 1))

         }



nestBoxes :: Int -> CL -> CL
nestBoxes n _
  | n < 0 =
      error "n >= 0"
nestBoxes 0 f = f
nestBoxes n f = BoxCL (singleAgent 1) (nestBoxes (n - 1) f)

nestingBox
  :: Int
  -> FamilyBenchmark

nestingBox  n
 | n < 1 =
      error "n >= 1"
 | otherwise =
      FamilyBenchmark
        { benchmarkFamily = "nestingBox"
        , benchmarkAgentUniverse = univAgent 2
        , benchmarkN = n
        , expectedProvable      = True
        , benchmarkFormula         =
            nestBoxes n topCL
        }




chain :: Int -> CL
chain n =
  conjunctionF
    ( [ BoxCL (singleAgent i)
          (ImpCL (pAtom i) (pAtom (i + 1)))
      | i <- [1 .. n - 1]
      ]
      ++
      [ BoxCL (singleAgent n) (pAtom 1) ]
    )

chainCL ::  Int -> FamilyBenchmark
chainCL  n
  | n < 2 =
      error "n >= 2"
  | otherwise =
      FamilyBenchmark
        { benchmarkFamily = "chainCL"
        , benchmarkAgentUniverse = univAgent n
        , benchmarkN = n
        , expectedProvable      = True
        , benchmarkFormula =
            ImpCL
              (chain n)
              (BoxCL (coalitionGrowth n) (pAtom n))
        }



nestedChainCL :: Int -> FamilyBenchmark
nestedChainCL n
  | n < 2 =
      error "n >= 2"
  | otherwise =
      FamilyBenchmark
        { benchmarkFamily = "nestedChainCL"
        , benchmarkAgentUniverse = univAgent (2 * n)
        , benchmarkN = n
        , expectedProvable = True
        , benchmarkFormula =
            ImpCL left right
        }
  where
    -- One nested implication link:
    -- [{n+i}][{i}](pi -> p(i+1))
    nested i =
      BoxCL
        (singleAgent (n + i))
        (BoxCL
          (singleAgent i)
          (ImpCL (pAtom i) (pAtom (i + 1))))

    -- The nested starting fact:
    -- [{2n}][{n}]p1
    startingFact =
      BoxCL
        (singleAgent (2 * n))
        (BoxCL
          (singleAgent n)
          (pAtom 1))

    -- All implication links together with the starting fact.
    left =
      conjunctionF
        ( [ nested i
          | i <- [1 .. n - 1]
          ]
          ++ [startingFact]
        )

    -- Inner agents: {1,...,n}
    innerCoalition =
      coalitionGrowth n

    -- Outer agents: {n+1,...,2n}
    outerCoalition =
      Set.fromList [n + 1 .. 2 * n]

    -- [{n+1,...,2n}][{1,...,n}]pn
    right =
      BoxCL
        outerCoalition
        (BoxCL innerCoalition (pAtom n))