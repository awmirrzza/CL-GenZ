{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wall #-}
module Logic.Coalition.Amir_CL where

import qualified Data.Set as Set
import GHC.Generics
import General
import Data.List (subsequences)

type Agent = Int
type Coalition = Set.Set Agent
type AgentUniverse = Set.Set Agent

fixedSetAG :: AgentUniverse
fixedSetAG = Set.fromList [1 .. 5]



-- Defining Coalition Logic Syntax
data CL = BotCL | AtCL Atom | ConCL CL CL | DisCL CL CL | ImpCL CL CL | BoxCL Coalition CL
  deriving (Eq,Ord,Generic)

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
  -- Axiom: Γ,p ⇒ ∆,p
  -- i would change these two lines of codes
  isAxiom _ fs _ = [ ("ax", [])| any (\f -> swap f `Set.member` fs) fs]
  -- Rule ⊥L: from Γ, ⊥ ⇒ ∆
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
  -- at pattern would make it nicer


{-
instance Arbitrary CL where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotCL, AtCL <$> elements (map return "pqrst")]
    genForm 1 = AtCL <$> elements (map return "pqrst")
    genForm n = oneof
      [ pure BotCL
      , AtCL <$> elements (map return "pqrst")
      , ImpCL <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConCL <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , DisCL <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , BoxCL <$> elements (map return "pqrst") <*> genForm (n `div` factor)
      ]
  shrink = nub . genericShrink -}
-- sequent calculus for coalition logic : safe rules(invertible)

safeCL :: Either CL CL -> [(RuleName, [Sequent CL])]
safeCL (Left (ConCL f g))   = [("andL", [Set.fromList [Left g, Left f]])]
safeCL (Left (DisCL f g))   = [("orL", [Set.singleton (Left f), Set.singleton (Left g)])]
safeCL (Left (ImpCL f g))   = [("implL", [Set.singleton (Right f), Set.singleton (Left g)])]
safeCL (Right (ConCL f g))  = [("andR", [Set.singleton (Right f), Set.singleton (Right g)])]
safeCL (Right (DisCL f g))  = [("orR", [Set.fromList [Right g, Right f]])]
safeCL (Right (ImpCL f g))  = [("implR", [Set.fromList [Right g, Left f]])]

safeCL _ = []

-- uses the fixed set of agents for the purpose of letting the code work and correctness, but can be modified to use a dynamic set of agents
cl :: Logic CL
cl = clWithAgents fixedSetAG
-- If I want another version, I can intentionally build one.
clWithAgents :: AgentUniverse -> Logic CL
clWithAgents dynamicSetAG
  | not (Set.null dynamicSetAG) =
      Log
        { name = "CL (" ++ show (Set.size dynamicSetAG) ++ " agents)"
        , safeRules = [leftBot, isAxiom, replaceRule safeCL]
        -- which coalitions are valid, and especially what counts as [Ag].
        , unsafeRules = [leftclRule dynamicSetAG, rightclRule dynamicSetAG]
        }
  | otherwise =
      error "a non-empty set of agents must be provided"


-- In CL, every box must use a coalition \(C\) such that: C ⊆ Ag
--if a coalition is valid or not for benchmarking
isvalidCoalition :: AgentUniverse -> Coalition -> Bool
isvalidCoalition dynamicSetAG coalition =
 coalition `Set.isSubsetOf` dynamicSetAG
-- accepts a complete CL formula and recursively checks everything inside




-- Walk through the whole formula and check every boxed coalition.
{-
boxclcheckFor
  uses isvalidformula
   then uses isvalidCoalition
-}
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
-- basecase nothing clashes
pairwiseDisjCL [] = True
-- recursion case
pairwiseDisjCL (c:cs) = all (disjCL c) cs && pairwiseDisjCL cs

-- {give me all non-empty SUBLISTS} Generate every non-empty subsequence/selection
nonEmptySubOf :: [a] -> [[a]]
nonEmptySubOf formulas = [ sub | sub <- subsequences formulas, not (null sub)]
-- if i can make this easier
findingCoalitionsLeft :: Sequent CL -> [(Coalition, CL)]
findingCoalitionsLeft currentseq = [(coalition, phi) | Left (BoxCL coalition phi) <- Set.toList currentseq]

findingCoalitionsRight :: Sequent CL -> [(Coalition, CL)]
findingCoalitionsRight currentseq = [(coalition, phi) | Right (BoxCL coalition phi) <- Set.toList currentseq]

-- also this easier
onlyCoalitions:: [(Coalition, CL)] -> [Coalition]
onlyCoalitions sequent = [coalition | (coalition, _) <- sequent]
-- also this easier
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
-- the version with Left (BoxCL _ _) can also be used and tested but it creates unnecessary duplicates rather than focus on the rule we
-- we are applying to the targeted boxformula
leftclRule dynamicSetAG _ currentseq (Left (BoxCL selectedCoalition selectedPhi)) =

  let
    -- 1. Find all left boxes.
    -- we get the only coalitions on the left side of the sequent
    leftCoalitions          =      findingCoalitionsLeft currentseq
    -- 2. Generate all non-empty selections.
    -- we remove the formulas without coalitions and keep only the coalitions and their formulas on the left side
    allLeftSubsetsSelected  = nonEmptySubOf leftCoalitions
    -- 3. Keep subsets only if coalitions are subsets of the ag passed whether the default one or the one the user passes and
      -- and pairwise-disjoint .
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
      -- 4. For each valid selection, create a CLLeft premise.
      subset <- findValidSubsets
      return ("CLcL", [premiseFromLeftConclusion subset])

leftclRule _ _ _ _ = []

rightclRule :: AgentUniverse  -> Rule CL
rightclRule dynamicSetAG _ currentseq (Right (BoxCL b theta))
  | not (isvalidCoalition dynamicSetAG b) = []
  | otherwise =

      let
        leftCoalitions =
          findingCoalitionsLeft currentseq

        validLeftSubs =
          [ subs
          | subs <- subsequences leftCoalitions
          , eachCcheck dynamicSetAG subs
          , pairwiseDisjCL (onlyCoalitions subs)
          , Set.unions (onlyCoalitions subs) `Set.isSubsetOf` b
          ]

        -- A right-side formula counts as [Ag]pi only when its coalition is
        -- exactly the current agent universe dynamicSetAG.
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
        leftSubset <- validLeftSubs
        rightpiSub <- piPossibilities
        return ("CLcR", [premiseRightRule leftSubset rightpiSub theta])

rightclRule _ _ _ _ = []