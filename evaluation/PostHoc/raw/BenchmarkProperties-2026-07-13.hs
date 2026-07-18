{-# OPTIONS_GHC -Wall #-}

module Logic.Coalition.Evaluation.Benchmark.BenchmarkProperties where
import qualified Data.Set as Set
import General (size)
import Logic.Coalition.Amir_CL
-- Number of boxes, atoms and connectives
numberofpieces :: CL -> Int
numberofpieces = size

-- -- Number of different agents in Ag, Agent Space
numberOfAgents :: AgentUniverse -> Int
numberOfAgents agentUniverse =
  Set.size agentUniverse


-- total number of boxes
numberofboxes :: CL -> Int
numberofboxes (AtCL _) =  0
numberofboxes BotCL    =  0
numberofboxes (ImpCL firstFormula secondFormula) =
  numberofboxes firstFormula + numberofboxes secondFormula

numberofboxes (DisCL firstFormula secondFormula) =
  numberofboxes firstFormula + numberofboxes secondFormula

numberofboxes (ConCL firstFormula secondFormula) =
  numberofboxes firstFormula + numberofboxes secondFormula

numberofboxes (BoxCL _ innerFormula) =
  1 + numberofboxes innerFormula


-- Deepest level of boxes placed inside boxes.
numberofdeepestBox :: CL -> Int
numberofdeepestBox (AtCL _) =  0
numberofdeepestBox BotCL    =  0
numberofdeepestBox (ImpCL firstFormula secondFormula)  =
    max (numberofdeepestBox firstFormula) (numberofdeepestBox secondFormula)
numberofdeepestBox (DisCL firstFormula secondFormula) =
    max (numberofdeepestBox firstFormula) (numberofdeepestBox secondFormula)
numberofdeepestBox (ConCL firstFormula secondFormula) =
    max (numberofdeepestBox firstFormula) (numberofdeepestBox secondFormula)
numberofdeepestBox (BoxCL _ innerFormula) =
   1 + numberofdeepestBox innerFormula





-- number of total Agents In Coalitions
allAgentsInCoalition :: CL -> Int
allAgentsInCoalition (AtCL _) =  0
allAgentsInCoalition BotCL    =  0
allAgentsInCoalition (ImpCL firstFormula secondFormula)  =
    allAgentsInCoalition firstFormula + allAgentsInCoalition secondFormula
allAgentsInCoalition (DisCL firstFormula secondFormula) =
    allAgentsInCoalition firstFormula + allAgentsInCoalition secondFormula
allAgentsInCoalition (ConCL firstFormula secondFormula) =
    allAgentsInCoalition firstFormula + allAgentsInCoalition secondFormula
allAgentsInCoalition (BoxCL coalition innerFormula) =
    Set.size coalition + allAgentsInCoalition innerFormula
