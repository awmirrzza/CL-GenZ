{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wall #-}
module Logic.Coalition.Amir_CL where

import qualified Data.Set as Set
import General
import Data.List (subsequences)

type Agent = Int
type Coalition = Set.Set Agent
type AgentUniverse = Set.Set Agent

fixedSetAG :: AgentUniverse
fixedSetAG = Set.fromList [1 .. 5]



-- Defining Coalition Logic Syntax
data CL = BotCL | AtCL Atom | ConCL CL CL | DisCL CL CL | ImpCL CL CL | BoxCL Coalition CL
  deriving (Eq,Ord)

-- Instance for showing CL formulas
instance Show CL where
  show BotCL       = "False"
  show (AtCL a)    =  a
  show (ConCL f g) = "(" ++ show f ++ " AND " ++ show g ++ ")"
  show (DisCL f g) = "(" ++ show f ++ " OR " ++ show g ++ ")"
  show (ImpCL f g) = "(" ++ show f ++ " - > " ++ show g ++ ")"
  show (BoxCL c f) = "[" ++ show c ++ "]" ++ show f


instance PropLog CL where
  neg f = ImpCL f BotCL
  con = ConCL
  dis = DisCL
  top = neg BotCL
  iff f g = ConCL (ImpCL f g) (ImpCL g f)
  isAtom (AtCL _) = True
  isAtom _        = False
  isAxiom _ fs _ = [ ("ax", [])| any (\f -> swap f `Set.member` fs) fs]
  leftBot _ fs _ = [ ("⊥L", []) | Left BotCL `Set.member` fs ]

  size BotCL       = 1
  size (AtCL _)    = 1
  size (ConCL f g) = 1 + size f + size g
  size (DisCL f g) = 1 + size f + size g
  size (ImpCL f g) = 1 + size f + size g
  size (BoxCL _ f) = 1 + size f

  subFormulas BotCL       = [BotCL]
  subFormulas (AtCL a)    = [AtCL a]
  subFormulas (ConCL f g) = ConCL f g : subFormulas f ++ subFormulas g
  subFormulas (DisCL f g) = DisCL f g : subFormulas f ++ subFormulas g
  subFormulas (ImpCL f g) = ImpCL f g : subFormulas f ++ subFormulas g
  subFormulas (BoxCL c f) = BoxCL c f : subFormulas f


safeCL :: Either CL CL -> [(RuleName, [Sequent CL])]
safeCL (Left (ConCL f g))   = [("andL", [Set.fromList [Left g, Left f]])]
safeCL (Left (DisCL f g))   = [("orL", [Set.singleton (Left f), Set.singleton (Left g)])]
safeCL (Left (ImpCL f g))   = [("implL", [Set.singleton (Right f), Set.singleton (Left g)])]
safeCL (Right (ConCL f g))  = [("andR", [Set.singleton (Right f), Set.singleton (Right g)])]
safeCL (Right (DisCL f g))  = [("orR", [Set.fromList [Right g, Right f]])]
safeCL (Right (ImpCL f g))  = [("implR", [Set.fromList [Right g, Left f]])]

safeCL _ = []

-- uses the fixed set of agents
cl :: Logic CL
cl = clWithAgents fixedSetAG

clWithAgents :: AgentUniverse -> Logic CL
clWithAgents dynamicSetAG
  | not (Set.null dynamicSetAG) =
      Log
        { name = "CL (" ++ show (Set.size dynamicSetAG) ++ " agents)"
        , safeRules = [leftBot, isAxiom, replaceRule safeCL]
        , unsafeRules = [leftclRule dynamicSetAG, rightclRule dynamicSetAG]
        }
  | otherwise =
      error "a non-empty set of agents must be provided"


-- C ⊆ Ag

isvalidCoalition :: AgentUniverse -> Coalition -> Bool
isvalidCoalition dynamicSetAG coalition =
 coalition `Set.isSubsetOf` dynamicSetAG
-- accepts a complete CL formula and recursively checks everything inside



isvalidformula :: AgentUniverse -> CL -> Bool
isvalidformula _ (AtCL _)               = True
isvalidformula  _ BotCL                 = True
isvalidformula dynamicSetAG (DisCL f g) = isvalidformula dynamicSetAG f && isvalidformula dynamicSetAG g
isvalidformula dynamicSetAG (ConCL f g) = isvalidformula dynamicSetAG f && isvalidformula dynamicSetAG g
isvalidformula dynamicSetAG (ImpCL f g) = isvalidformula dynamicSetAG f && isvalidformula dynamicSetAG g
isvalidformula dynamicSetAG (BoxCL coalition f) = isvalidCoalition dynamicSetAG coalition && isvalidformula dynamicSetAG f

fixedboxclcheck :: CL -> Either String Bool
fixedboxclcheck = boxCheck fixedSetAG

boxCheck :: AgentUniverse  -> CL -> Either String Bool
boxCheck dynamicSetAG f
  | Set.null dynamicSetAG =
    Left "Provide non-empty set of agents for the CL"
  -- |isvalidformula f = Right (isProvableZ cl f) for the correctness, we used this line
  | isvalidformula dynamicSetAG f =
    Right (isProvableZ (clWithAgents dynamicSetAG) f)
  | otherwise =
    Left "the coalition is not a subset of Ag, therefore invalid formula"

-- Coalition helpers
disjCL :: Coalition -> Coalition -> Bool
disjCL c1 c2 = Set.null (Set.intersection c1 c2)

pairwiseDisjCL:: [Coalition] -> Bool
pairwiseDisjCL [] = True
pairwiseDisjCL (c:cs) = all (disjCL c) cs && pairwiseDisjCL cs

--  all non-empty SUBLISTS
nonEmptySubOf :: [a] -> [[a]]
nonEmptySubOf formulas = [ sub | sub <- subsequences formulas, not (null sub)]

findingCoalitionsLeft :: Sequent CL -> [(Coalition, CL)]
findingCoalitionsLeft currentseq = [(coalition, phi) | Left (BoxCL coalition phi) <- Set.toList currentseq]

findingCoalitionsRight :: Sequent CL -> [(Coalition, CL)]
findingCoalitionsRight currentseq = [(coalition, phi) | Right (BoxCL coalition phi) <- Set.toList currentseq]

onlyCoalitions:: [(Coalition, CL)] -> [Coalition]
onlyCoalitions sequent = [coalition | (coalition, _) <- sequent]

onlyFormulas :: [(Coalition, CL)] -> [CL]
onlyFormulas sequent = [phi | (_, phi) <- sequent]

premiseFromLeftConclusion :: [(Coalition, CL)] -> Sequent CL
premiseFromLeftConclusion subset =
  Set.fromList
    [ Left phi
    | (_, phi) <- subset
    ]
premiseRightRule :: [(Coalition, CL)] -> [CL] -> CL -> Sequent CL
premiseRightRule leftForm piSub theta = Set.fromList (left ++ right ++ bInnerF)
   where
    left = map Left (onlyFormulas leftForm)
    right = map Right piSub
    bInnerF = [Right theta]

eachCcheck :: AgentUniverse -> [(Coalition, CL)] -> Bool
eachCcheck dynamicSetAG subset =
  and
    [ isvalidCoalition dynamicSetAG coalition
    | (coalition, _) <- subset
    ]


leftclRule :: AgentUniverse  -> Rule CL
-- the version with Left (BoxCL _ _) can also be used and tested but it creates unnecessary duplicates which damagers runtime rather than focus on the rule we
-- we are applying to the targeted boxformula
leftclRule dynamicSetAG _ currentseq (Left (BoxCL selectedCoalition selectedPhi)) =

  let
    --  all left boxes.
    leftCoalitions          =      findingCoalitionsLeft currentseq
    -- 2. all non-empty left selections.
    allLeftSubsetsSelected  = nonEmptySubOf leftCoalitions
    -- 3. C ⊆ B &  pairwise-disjoint .
    findValidSubsets        = [ subset  | subset <- allLeftSubsetsSelected
                          , eachCcheck dynamicSetAG subset
                          , pairwiseDisjCL (onlyCoalitions subset)
                          ]


  in
    if null leftCoalitions
    then []
    else if not ((selectedCoalition, selectedPhi) == head leftCoalitions)
    then []
    else do
      -- CLLeft premise.
      subset <- findValidSubsets
      return ("CLcL", [premiseFromLeftConclusion subset])

leftclRule _ _ _ _ = []

rightclRule :: AgentUniverse  -> Rule CL

rightclRule dynamicSetAG _ currentseq (Right (BoxCL b theta))

  -- b should contain valid agents
  | not (isvalidCoalition dynamicSetAG b) = []
  | otherwise =
      let
        -- all left coalitions
        leftCoalitions =
          findingCoalitionsLeft currentseq
        -- n = 0, valid, pairwise , U C ⊆ B
        validLeftSubs =
          [ subs
          | subs <- subsequences leftCoalitions
          , eachCcheck dynamicSetAG subs
          , pairwiseDisjCL (onlyCoalitions subs)
          , Set.unions (onlyCoalitions subs) `Set.isSubsetOf` b
          ]

        -- [Ag]pi
        grandCoalitionAG =
          [ (coalition, formula)
          | (coalition, formula) <- findingCoalitionsRight currentseq
          , coalition == dynamicSetAG
          , not (coalition == b && formula == theta)
          ]

        grandFormulas   =
          onlyFormulas grandCoalitionAG

        piPossibilities =
          subsequences grandFormulas

      in do
      --  CL right premise
        leftSubset <- validLeftSubs
        rightpiSub <- piPossibilities
        return ("CLcR", [premiseRightRule leftSubset rightpiSub theta])


rightclRule _ _ _ _ = []