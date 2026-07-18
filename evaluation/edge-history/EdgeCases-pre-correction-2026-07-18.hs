{-# OPTIONS_GHC -Wall #-}
module Logic.Coalition.Evaluation.EdgeCases where
import qualified Data.Set as Set
import General
import Logic.Coalition.Amir_CL

p, q, r :: CL
p = AtCL "p"
q = AtCL "q"
r = AtCL "r"

c3, c4, c34, cInvalid :: Coalition
c3 = Set.fromList [3]
c4 = Set.fromList [4]
c34 = Set.fromList [3, 4]
cInvalid = Set.fromList [10] -- Outside Agent Universe

emptyCoalition :: Coalition
emptyCoalition = Set.empty

emptyp, emptyq, emptyr :: CL
emptyp = BoxCL emptyCoalition p
emptyq = BoxCL emptyCoalition q
emptyr = BoxCL emptyCoalition r

box3p, box3q, box3r, box4p, box4q, box4r, box34p, box34q, box34r :: CL
box3p = BoxCL c3 p
box3q = BoxCL c3 q
box3r = BoxCL c3 r
box4p = BoxCL c4 p
box4q = BoxCL c4 q
box4r = BoxCL c4 r
box34p = BoxCL c34 p
box34q = BoxCL c34 q
box34r = BoxCL c34 r

negF :: CL -> CL
negF f = ImpCL f BotCL

topF :: CL
topF = negF BotCL


-- Pairwise Disjoint for CL left
condition1 :: CL
condition1 =
  negF
    (ConCL
      box3p
      (BoxCL emptyCoalition (negF p))
    )
--  m >= 1 for Left rule
condition2 :: CL
condition2 =  ImpCL emptyq BotCL

-- Rejects overlapping selections for CL left
condition3 :: CL
condition3 = negF (ConCL box3p (BoxCL c34 (negF p)))

-- CL left =  empty coalition
condition4 :: CL
condition4 =
  ImpCL
    (BoxCL emptyCoalition BotCL)
    BotCL

-- n >= 0 = cl right
condition5 :: CL
condition5 = BoxCL c4 topF

-- Pairwise-disjoint = CL right
condition6 :: CL
condition6 = ImpCL (ConCL box3p box4q) (BoxCL c34 (ConCL p q))

-- Rejects overlapping selections - CL Right
condition7 :: CL
condition7 = ImpCL (ConCL box4p box4q) (BoxCL c4 (ConCL p q))

-- 8.  B = Ag - cl right
condition8 :: CL
condition8 = BoxCL fixedSetAG topF

-- 9. B = Empty-  cl right
condition9 :: CL
condition9 = BoxCL emptyCoalition topF

-- (∅ ∩ ∅ = ∅) cl right
condition10 :: CL
condition10 = ImpCL (ConCL emptyp emptyq) (BoxCL emptyCoalition (ConCL p q))

-- invalid coalition
condition11 :: Bool
condition11 = isvalidformula fixedSetAG (BoxCL cInvalid r)


cases :: [(String, CL, Bool)]
cases =
  [ ("condition1",  condition1,  True)
  , ("condition2",  condition2,  False)
  , ("condition3",  condition3,  False)
  , ("condition4",  condition4,  True)
  , ("condition5",  condition5,  True)
  , ("condition6",  condition6,  True)
  , ("condition7",  condition7,  False)
  , ("condition8",  condition8,  True)
  , ("condition9",  condition9,  True)
  , ("condition10", condition10, True)
  ]


main :: IO ()
main = do
  putStrLn "propertyName,isValid,isExpected,genZ,isAgreed"
  mapM_ printResult cases
  putStrLn ("Condition11," ++ show condition11)
  where
    printResult (propertyName, formula, isExpected) = do
      let ag = clWithAgents fixedSetAG
          isValid = isvalidformula fixedSetAG formula
          proofbyGenZ = isProvableZ ag formula
          isAgreed =
            isValid && proofbyGenZ == isExpected
      putStrLn
        ( propertyName
            ++ "," ++ show isValid
            ++ "," ++ show isExpected
            ++ "," ++ show proofbyGenZ
            ++ "," ++ show isAgreed
        )