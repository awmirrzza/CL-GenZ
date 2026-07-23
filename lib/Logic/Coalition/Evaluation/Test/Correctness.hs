{-# OPTIONS_GHC -Wall #-}

module Logic.Coalition.Evaluation.Test.Correctness where
import qualified Data.Set as Set
import General
import Logic.Coalition.Amir_CL
p, q :: CL
p = AtCL "p"
q = AtCL "q"

c3 :: Coalition
c3 = Set.fromList [3]

negF :: CL -> CL
negF f = ImpCL f BotCL


l1 :: CL
l1 = BoxCL (Set.singleton 3) (negF BotCL)

l1prime :: CL
l1prime = negF (BoxCL (Set.singleton 3) (negF BotCL))

s1 :: CL
s1 = negF (BoxCL (Set.singleton 3) BotCL)


s1prime :: CL
s1prime = BoxCL (Set.singleton 3) BotCL

om1 :: CL
om1 = ImpCL (BoxCL (Set.singleton 3) (ConCL p q))
        (BoxCL (Set.singleton 3) p)

om1prime :: CL
om1prime = ImpCL (BoxCL (Set.singleton 3) p)
        (BoxCL (Set.singleton 3) (ConCL p q))

sa1 :: CL
sa1 =
  ImpCL
    (ConCL
      (BoxCL (Set.singleton 3) p)
      (BoxCL (Set.singleton 4) q))
    (BoxCL (Set.fromList [3,4]) (ConCL p q))

sa1prime :: CL
sa1prime =
  ImpCL
    (BoxCL (Set.fromList [3,4]) (ConCL p q))
    (ConCL
      (BoxCL (Set.singleton 3) p)
      (BoxCL (Set.singleton 4) q))


agmax1 :: CL
agmax1 =
  ImpCL
    (negF (BoxCL (Set.empty) p))
    (BoxCL (fixedSetAG) (negF p))

agmax1prime :: CL
agmax1prime = ImpCL
    (negF (BoxCL (Set.empty) p))
    (BoxCL (c3) (negF p))


cm1 :: CL
cm1 =
  ImpCL
    (BoxCL (Set.fromList [3]) p)
    (BoxCL (Set.fromList [3, 4]) p)

cm1prime :: CL
cm1prime =
  ImpCL
  (BoxCL (Set.fromList [3, 4]) p)
  (BoxCL (Set.fromList [3]) p)




cases :: [(String, CL, Bool)]
cases =
  [ ("L1", l1, True)
  , ("L1'", l1prime, False)
  , ("S1", s1, True)
  , ("S1'", s1prime, False)
  , ("OM1", om1, True)
  , ("OM1'", om1prime, False)
  , ("SA1", sa1, True)
  , ("SA1'", sa1prime, False)
  , ("AGMax1", agmax1, True)
  , ("AGMax1'", agmax1prime, False)
  , ("CM1", cm1, True)
  , ("CM1'", cm1prime, False)
  ]
main :: IO ()
main = do
  putStrLn "propertyName,isValid,isExpected,genZexpected,genZ,genTexpected,genT,isAgreed"
  mapM_ printResult cases
  where
    printResult (propertyName, formula, isExpected) = do
      let ag = clWithAgents fixedSetAG
          isValid = isvalidformula fixedSetAG formula
          proofbyGenT = isProvableT ag formula
          proofbyGenZ = isProvableZ ag formula
          genZexpected = proofbyGenZ == isExpected
          genTexpected = proofbyGenT == isExpected
          isAgreed =
            isValid
              &&  genZexpected
              &&  genTexpected

      putStrLn
        ( propertyName
            ++ "," ++ show isValid
            ++ "," ++ show isExpected
            ++ "," ++ show genZexpected
            ++ "," ++ show proofbyGenZ
            ++ "," ++ show genTexpected
            ++ "," ++ show proofbyGenT
            ++ "," ++ show isAgreed
        )