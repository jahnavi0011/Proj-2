--Name : Jahnavi D
--Student id: 1337457
--Purpose: COMP30020 Declarative Programming, Project 2

-- Proj2 is a two player guessing game similar to BattleShip, where the
-- 'searcher' tries to locate three battleships hidden by the 'hider' across 
-- a 4x8 grid, and each grid location is identified by an uppercase letter
-- [A-H], followed by an integer [1-4]. 

-- The searcher makes a guess consisting of 3 distinct locations, and the three 
-- hidden ships must be located in a single guess. The objective is to find 
-- the exact locations using least number of guesses.  


-- The hider and searcher parts of the game are implmented using:
-- intialGuess - makes an intial guess and an intial gamestate contains all
--               possible target candidates

-- nextGuess - makes the next guess based on feedback received using the feedback
--             function and previous guesses and gamestate

-- feedback - provides feedback for given target candidate and guess based on 
--            how close it is to the target.

-- toLocation - a coverter function used to convert a String to a valid 
--              Location type of (Char,Int)

-- fromLocatiom - a converter function used to convert a Location to a
--               String type

-- GameState - GameState stores all possible targets and is frequently pruned
--            based on the guesses made 

-- Location - Location is a representation of grid location as (Char,Int) 

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char
import Data.Ord
import Data.List 
-- for unit testing functions , particularly feedback, intialGuess and nextGuess
import Debug.Trace 


-- Location is of type (Char,Int)
type Location = (Char,Int)

-- function that converts given string '[A-Z][1-4]' to a valid location 
-- or returns Nothing
toLocation :: String -> Maybe Location
toLocation [col,row]
  | elem col ['A'..'H'] && elem row ['1'..'4'] = Just (col, digitToInt row)
  | otherwise = Nothing

-- function that converts a valid location to string type
fromLocation :: Location -> String
fromLocation (col,row) = [col] ++ [intToDigit row]

-- gameState to keep track of potential targets between guesses
type GameState = [[Location]]

-- list of all possible targets : combination of all positions 
allTargets :: [[Location]]
allTargets = combinations 3 [(col,row) | col <- ['A'..'H'], row <- [1..4]]

-- provides a list of all possible length n combinations, of elements in a list 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']


-- feedback function to determine accuracy of guesses made.
-- it takes a list of targets and guesses and returns the feedback as a triplet
-- (correctLoc,oneSpaceAway, twoSpaceAway) where spaces away is the no of blocks
-- away from the correct location.
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback targets guess = 
  (length correct, length (oneSqCount \\ correct), length ((twoSqCount \\ oneSqCount) \\ correct))
    where correct = nub [g | g <- guess, t <- targets, g == t]
          oneSqCount = nub [g | g <- guess, t <- targets, oneSq g t]
          twoSqCount = nub [g | g <- guess, t <- targets, twoSq g t]
 

 
-- function that computes if a given location is one space away from target loc
-- takes two Locations and calculates differnce using ord and abs
oneSq :: Location -> Location -> Bool
oneSq (col1,row1) (col2,row2) = 
  (abs((ord col1) - (ord col2)) <= 1) && (abs(row1 - row2) <= 1) 
 
-- function that computes if a given location is two spaces away from target loc
twoSq :: Location -> Location -> Bool
twoSq (col1,row1) (col2,row2) = 
  (abs((ord col1) - (ord col2)) <= 2) && (abs(row1 - row2) <= 2) 
  
-- intialGuess gives the intial guess and gamestate containing every
-- distinct target combination possible except the intial guess made.
-- the intial guess was chosen arbitarly and tested on multiple test cases
-- but a logical approach , preferably a simulation that determines 
-- bestInitialGuess by testing an intial guess against all possible combinations
-- would be effective
initialGuess :: ([Location],GameState)
initialGuess = (guess, allTargets \\ [guess])
  where guess = [('B',2),('F',2),('G',3)]


-- nextGuess Remove targets inconsistent with the received feedback from the
-- gamestate and only guesses which would have resulted in same feedback as
-- the prevGuess are preserved
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (prevGuess, remainingTargets) feedbackReceived = 
    (newGuess, newGameState)
  where 
    newGameState = filter (consistentWithFeedback feedbackReceived prevGuess) remainingTargets
    newGuess = bestGuess newGameState

-- checks if a target is consistent with received feedback for a given guess
consistentWithFeedback :: (Int, Int, Int) -> [Location] -> [Location] -> Bool
consistentWithFeedback val guess target = val == feedback target guess

-- BestGuess gives the best possible guess based on the remaing targets in the 
-- gamestate by choosing the guess combination that would result in least 
-- possible targets to remain in gamestate, reducing the
-- problem space significantly
bestGuess :: GameState -> [Location]
bestGuess targets = fst $ minimumBy (comparing snd) getExpectationList
  where 
    getExpectationList = [(guess, getExpectation guess targets) | guess <- targets]

--this function determines how 'informative' a guess is, if the expectation of
--the guess is minimised it is the 'best' guess as it leaves fewer possible
--guesses on average, and speeds up gameplay


-- for every possible target configuration, we compute the feedback obtained 
-- if our guess were to be used against it.
-- these feedback results are grouped so that we know its frequency.
-- we compute the expectation by summing over all feedback types. 
-- for each feedback type:
--    the probability of that feedback occurring is calculated which 
--      is the number of times that feedback occurs divided by the total
--      number of targets.
--    this is  multiplied  by the number of targets that give that feedback 
--       which  gives the average number of possibilities we'd be left with if 
--       we received that feedback.
-- summing over all feedback types gives the overall expected number of possibilities 
--       left after the guess.
getExpectation :: [Location] -> GameState -> Double
getExpectation guess targets = 
  sum [(fromIntegral count / fromIntegral total) * (fromIntegral count) | (feedbackReceived, count) <- groupedFeedback]
  where
    groupedFeedback = map (\list -> (head list, length list)) $ group $ sort feedbacks
    feedbacks = map (feedback guess) targets
    total = length feedbacks
