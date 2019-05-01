-- Author: Xinjie Lan (xinjiel2) <xinjiel2@student.unimelb.edu.au>
-- Purpose: Play the game of musician which gusses Pitches

--This program has both composer and performer parts. The composer select
-- begins by selecting a three-pitch musical chord, where each pitch 
--comprises a musical note from letter A to G, and an octave, from number
-- one to three. 
--Once the composer has selected the target chord, the performer repeatedly
-- chooses a similarly defined chord as a guess and tells it to the 
--composer, who responds by giving the performer the feedback. 
--The game finishes when the performer guess the right pitches.


module Proj1 (Pitch, toPitch, feedback, 
   GameState, initialGuess, nextGuess) where

import Data.List

--Define a Note and Octave type 
data Note = A|B|C|D|E|F|G deriving (Eq, Ord)
data Octave = One|Two|Three deriving (Eq, Ord)

--Define a Pitch type with consturctor Pitch. 
--Two arguments are Maybe Note and Maybe Octave
data Pitch = Pitch (Maybe Note) (Maybe Octave) deriving (Eq, Ord)

--The type for Gamestate is a list of Pitch list which is 
--all the possible combinations of pitches.
type GameState = [[Pitch]]




--This section of code are the helper functions for toPitch which converts
--types between String, Note, Octave etc.

--This function converts a Note type to String by enumerate eight consturctors
noToString :: Note -> String
noToString A = "A"
noToString B = "B"
noToString C = "C"
noToString D = "D"
noToString E = "E"
noToString F = "F"
noToString G = "G"
--This function converts a letter into Maybe Note type. 
toNote :: [Char] -> Maybe Note
toNote "A" = Just A
toNote "B" = Just B
toNote "C" = Just C
toNote "D" = Just D
toNote "E" = Just E
toNote "F" = Just F
toNote "G" = Just G
toNote _ = Nothing

--This function converts Octave to String
ocToString :: Octave -> String
ocToString One = "1"
ocToString Two = "2"
ocToString Three = "3"
--This function converts String(Char list) to Octave type
toOctave :: [Char] -> Maybe Octave 
toOctave "1" = Just One
toOctave "2" = Just Two
toOctave "3" = Just Three
toOctave _ = Nothing

--toPitch function which takes a String and returns Maybe Pitch
--This function checks if the input follows a valid format
--Then call the two helper functions to convert the string to Maybe Note
--and Maybe Octave type in order to form the Pitch type.
toPitch :: String -> Maybe Pitch
toPitch "" = Nothing
toPitch (t:ts) 
    | length(t:ts) /= 2 = Nothing
    | (t:ts) == showPitch (Pitch (toNote [t]) (toOctave ts)) 
        = Just(Pitch (toNote [t]) (toOctave ts))
    | otherwise = Nothing

--Special show function to represent Pitch type
--Called two helper function above to convert Note and Octave type to string
--Then combined the results together to form a Pitch value in String
showPitch :: Pitch -> String
showPitch (Pitch (Just t) (Just s)) = noToString t ++ ocToString s
showPitch (Pitch Nothing Nothing) = ""
showPitch (Pitch Nothing (Just s)) = ""
showPitch (Pitch (Just t) Nothing) = ""

--Declaring Pitch to Show class and spefcify the show fucntion to showPitch
instance Show Pitch where show = showPitch 




--This section is related to feedback function 
--and the corresponding helper functions

--Feedback function which takes an target and guess to generate the feedback
--This functions called three helper functions for each comparing result
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int) 
feedback [] [] = (0,0,0)
feedback (x:xs) [] = (0,0,0)
feedback [] (x:xs) = (0,0,0)
feedback (x:xs) (t:ts) = (toPitchCount (x:xs) (t:ts),
   toNoteCount (x:xs) (t:ts) - pitcCount, 
   toOctaveCount (x:xs) (t:ts)- pitcCount) 
      where pitcCount 
               = toPitchCount (x:xs) (t:ts)

--This function calculate the number of correct pitches by comparing
--target pitch list and guessing pitch list
toPitchCount :: [Pitch] ->[Pitch] -> Int
toPitchCount (x:xs) [] = 0
toPitchCount [] (t:ts) = 0
toPitchCount (x:xs) (t:ts) = length(filter (==x) (t:ts)) 
   + toPitchCount xs (t:ts)

--This function takes pitch lists from target and guess 
--parse them and extract Note from ptiches.
--This is achieved by calling showPitch function to convert pitch
-- to string first then take the first element as input to toNote function
--Then this element became to a Maybe Note type 
--then add into the Maybe Notelist
toNoteList :: [Pitch] -> [Maybe Note]
toNoteList [] = []
toNoteList (x:xs) =  toNote [(head (showPitch x))] : toNoteList xs

--This function calculates how many correct Note in the guess pitch list.
--It first generates a list of Note from target(List A)
--Then generates a list of Note from guess(List B)
--Deleting the elements that are same in both lists from the List A
--Now using the length of target subtract the lenght of calculated List A
--This is the number of corret Notes in the guess list. 
--But it didn't check for element that are correct in Pitch. 
--Return this number.
toNoteCount :: [Pitch] -> [Pitch] -> Int
toNoteCount [] [] = 0
toNoteCount (x:xs) [] = 0
toNoteCount [] (t:ts) = 0
toNoteCount (x:xs) (t:ts) = length(x:xs) - 
   length(toNoteList (x:xs) \\ toNoteList (t:ts))

--Similar to toNoteList function, 
--but this time take last element from showPitch
toOctaveList :: [Pitch] -> [Maybe Octave]
toOctaveList [] = []
toOctaveList (x:xs) =  [toOctave [(last (showPitch x))]] ++ toOctaveList xs

--Same logic as toNoteCount(), but this funtion deal with Octave type
toOctaveCount :: [Pitch] -> [Pitch] -> Int
toOctaveCount [] [] = 0
toOctaveCount (x:xs) [] = 0
toOctaveCount [] (t:ts) = 0
toOctaveCount (x:xs) (t:ts) = length(x:xs) - 
   length(toOctaveList (x:xs) \\ toOctaveList (t:ts))

 


--This section is related to guess functions.
--I teseted several cases, and the optimal case 
--is (C3,F1,B2). Below are the results of some test cases.
-- (A1,A2,A3)-->5.4
-- (A1,B2,A3)-->4.85
-- (A1,B2,B3)--> 4.9
-- (C1,B2,A3)-->4.51
-- (C2,B2,A3)-->4.42
-- (C3,B2,A3)-->4.467
-- (D2,B2,A3) -->4.61
-- (C2,B2,A2) -->4.45
-- (C2,B2,C3) --> 5.067
-- (C2,B3,A3) -->4.583
-- (C2,B1,A3) -->4.41
-- (C3,F1,B2) -->4.38

--Initial Guess function which takes a list of pitches as guess
--and a gamestate which contains all the states of possible solutions
initialGuess :: ([Pitch],GameState)
initialGuess = ([(Pitch (Just C) (Just Three)), 
   (Pitch (Just F) (Just One)), 
   (Pitch (Just B) (Just Two))],
      allPossiblePitches)

--This function takes guess from last results and its feedback
--then generate next guess by using
--the first element of the possible filted solution list.
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) ->([Pitch],GameState)
nextGuess ([],[]) (_,_,_) = ([],[])
nextGuess ((x:xs),[]) (_,_,_) = ([],[])
nextGuess ([],((x:xs))) (_,_,_) = ([],[])
nextGuess ((x:xs),((t:ts))) (3,0,0) = ([],[])
nextGuess ((x:xs),((t:ts))) (a,b,c) = 
   (  next ,(filtTargets (x:xs) ((t:ts)) [] (a,b,c))) 
      where next = 
               getSinglePitch ((filtTargets (x:xs) ((t:ts)) [] (a,b,c)))

--This function filters the remaining solutions by only taking
--the same feedback as the last guesses from the gamestate.   
filtTargets :: [Pitch] -> GameState -> [[Pitch]]-> (Int,Int,Int)-> GameState
filtTargets [] [] [] (_,_,_) = []
filtTargets (x:xs) [] [] (_,_,_) = []
filtTargets (x:xs) [] ((y:ys)) (_,_,_) = ((y:ys))
filtTargets (x:xs) ((t:ts)) [] (a,b,c)
   | compareTuple (feedback (x:xs) t) (a,b,c) 
      = filtTargets (x:xs) ts ([t]++[])  (a,b,c)
   | otherwise = filtTargets (x:xs) ts []  (a,b,c)
filtTargets (x:xs) ((t:ts)) ((y:ys)) (a,b,c)
   | compareTuple (feedback (x:xs) t) (a,b,c) 
      = filtTargets (x:xs) ts ([t]++(y:ys))  (a,b,c)
   | otherwise = filtTargets (x:xs) ts ((y:ys))  (a,b,c)
 
--Helper function is used to compare if the feedback 
-- of two feedback values are the same, return True or False
compareTuple :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
compareTuple (a,b,c) (e,f,g)
    | a == e && b == f && c==g = True
    | otherwise  = False

    
--This function is helper function which generates all the possible pitches
--For example, it produces A One, A Two till G Three
getAllPitch :: [Pitch]
getAllPitch = [Pitch i j | 
   i <- [Just A,Just B,Just C,Just D,Just E,Just F,Just G], 
   j <- [Just One,Just Two,Just Three] ]
--This function is helper function 
--which only takes the first element from a list
getSinglePitch :: [[ Pitch]] -> [Pitch]
getSinglePitch  ((x:  xs)) =  x
--This function produces all the possible combinations of the pitches
--The results are 1330 different combinations
allPossiblePitches :: [[ Pitch]]
allPossiblePitches = [[a,b,c]| a <- getAllPitch, 
   b <-[d| d <- getAllPitch, 
   d /= a, d<a], 
   c <- [e| e <- getAllPitch, 
   e /= a, e/= b, e<a,e<b]]