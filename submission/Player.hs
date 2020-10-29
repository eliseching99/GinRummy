-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards
    ( Card(..),
      Rank(Jack, Ace, King, Two, Three,Four, Five,Six,Seven, Eight, Nine,Queen,Ten),
      Suit(Club, Diamond, Spade, Heart) )         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import EitherIO
import Data.List
import Data.Ord

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
-- type ActionFunc
--   = Card            -- ^ card on top of the discard pile
--   -> (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> Maybe String
--   -- ^ player's memory, on first player turn in the first round it will be Nothing
--   -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
--   -> [Card]     -- ^ the player's hand
--   -> (Draw, String) -- ^ which pile did the player chose to draw from and memory
pickCard :: ActionFunc
pickCard _ _ _ _ _ =(Stock,"yep")

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.

-- A player receives the card he decided to draw (from discard or stock), her
-- hand and her memory. She then choses whether to Knock or Discard.
-- type PlayFunc
--   = Card              -- ^ picked card
--   -> (Score, Score)   -- ^ scores of (player, opponent) as of last round
--   -> String           -- ^ the player's memory
--   -> [Card]           -- ^ the player's hand (without new card)
--   -> (Action, String) -- ^ the player's chosen card and new memory
playCard :: PlayFunc
-- playCard _ _ _ deck = ( Action Drop a ,"lol") where a = deck!!0
playCard pickedCard _ _ deck 
    -- |length deadwoodcards==0 && 
    |length deadwoodcards==1 && discardedCardforGin/=pickedCard = (Action Gin discardedCardforGin ,"lol") 
    |totalDeadwoodPoints deadwoodcards >=10 = (Action Drop highestcard, "lol")
    |totalDeadwoodPoints deadwoodcards <10 && deadwoodCardDiscard/=pickedCard = (Action Knock deadwoodCardDiscard,"lol")
    |otherwise = (Action Drop highestcard, "lol")
    
    where 
        highestcard= last (sort (deck))
        deckPlusNewCard = deck++[pickedCard]
        meldsFormed = (makeMelds (30,30) "lol" deckPlusNewCard)
        deadwoodcards= filter(isDeadwood) meldsFormed


        discardedCardforGin=  last (convertMeldtoCard(last deadwoodcards))
        deadwoodCardDiscard = last (convertMeldtoCard (maxByRankMeld deadwoodcards))
        convertDeadwoodMeldToCards= concat (map (convertMeldtoCard) deadwoodcards)
        deadwoodwithoutpickedCard= convertDeadwoodMeldToCards\\ [pickedCard]
        maxDeadwoodCardForKnock= if length deadwoodwithoutpickedCard>0 then last(sort(deadwoodwithoutpickedCard)) else highestcard
        -- maxDeadwoodCardForKnock= last (sort (deadwoodwithoutpickedCard))


checkIfFourFive::Meld->Int
checkIfFourFive (Set4 _ _ _ _)=4
checkIfFourFive (Straight4 _ _ _ _)=4
checkIfFourFive (Straight5 _ _ _ _ _)=5

--check if can form gin with current hand +
-- 

-- Get the rank of the card
getRankMeld :: Meld-> Rank
getRankMeld (Deadwood (Card _ r)) = r
getRankMeld (Set3 _ _ _)= Ace
getRankMeld(Set4 _ _ _ _)=Ace
getRankMeld(Straight3 _ _ _)=Ace
getRankMeld(Straight4 _ _ _ _)=Ace
getRankMeld(Straight5 _ _ _ _ _)=Ace

-- Getting max card
maxByRankMeld :: [Meld] -> Meld
maxByRankMeld= foldr1 (\meld_a meld_b -> if getRankMeld meld_a>= getRankMeld meld_b then meld_a else meld_b)

convertMeldtoCard:: Meld -> [Card]
convertMeldtoCard (Deadwood (Card s r)) = [(Card s r)]
convertMeldtoCard (Set3 _ _ _) = []
convertMeldtoCard (Set4 _ _ _ _) = []
convertMeldtoCard (Straight3 _ _ _) = []
convertMeldtoCard (Straight4 _ _ _ _) = []
convertMeldtoCard (Straight5 _ _ _ _ _) = []

--get maximum deadwood as discarded card


-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
-- Which melds to use for scoring.
-- type MeldFunc
--   = (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> String        -- ^ the player's memory
--   -> [Card]        -- ^ cards in player's hand
--   -> [Meld]        -- ^ elected melds
-- makeMelds :: MeldFunc
-- makeMelds _ _ deck = convertHandtoDeadwood deck

--TESTING A WORKS PARTIALLY
-- makeMelds :: MeldFunc
-- makeMelds _ _ deck 
--     | length (formAllSetMeld deck) > length(getStraightForAllSuit deck)  = formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
--     | length (getStraightForAllSuit deck)> length (formAllSetMeld deck) = getStraightForAllSuit deck ++ formAllSetMeld discardStraightNoSet ++ convertHandtoDeadwood finaldeadwood
--     | length (getStraightForAllSuit deck)==0  && length(formAllSetMeld deck)== 0 = convertHandtoDeadwood deck
--     | otherwise = convertHandtoDeadwood deck
--     where 
--         remainder = deck \\ remainderCards deck
--         discardStraight = remainder \\ getDiscardofStraight remainder

--         discardStraightNoSet = deck\\getDiscardofStraight deck
--         finaldeadwood= discardStraightNoSet \\ remainderCards discardStraightNoSet
--TESTING B TESTINGGGG
makeMelds :: MeldFunc
makeMelds _ _ deck 
    | length (formAllSetMeld deck) > length(getStraightForAllSuit deck)  = formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
    | length (getStraightForAllSuit deck)> length (formAllSetMeld deck) = getStraightForAllSuit deck ++ formAllSetMeld discardStraightNoSet ++ convertHandtoDeadwood finaldeadwood
    | length(formAllSetMeld deck)== length (getStraightForAllSuit deck )= formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
    | length (getStraightForAllSuit deck)==0  && length(formAllSetMeld deck)== 0 = convertHandtoDeadwood deck
    | otherwise = convertHandtoDeadwood deck
    where 
        remainder = deck \\ remainderCards deck
        discardStraight = remainder \\ getDiscardofStraightTest remainder

        discardStraightNoSet = deck\\getDiscardofStraightTest deck
        finaldeadwood= discardStraightNoSet \\ remainderCards discardStraightNoSet




-- Takes meld as input and checks if its not a Deadwood
isNotDeadwood :: Meld -> Bool
isNotDeadwood (Deadwood _) = False
isNotDeadwood _     = True

-- Takes meld as input and checks if it is Deadwood
isDeadwood :: Meld -> Bool
isDeadwood (Deadwood _) = True
isDeadwood _ = False


-- formAllSetMeld deck + (getStraightForAllSuit )

-- bmiTell :: (RealFloat a) => a -> String  
-- bmiTell bmi  
--     | bmi <= 18.5 = "You're underweight, you emo, you!"  
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
--     | otherwise   = "You're a whale, congratulations!"  






-- zipWith (\x y -> 2*x + y) [1..4] [5..8]
-- Player hand = [Card Club Two, Card Heart Two, Card Spade Two, Card Heart Ace, Card Club Ace, Card Spade Ace, Card Club Five, Card Club Seven , Card Club Nine,Card Club Ten]
remainderCards :: [Card]-> [Card]
remainderCards deck= concat (filter(not.null) $ checkAllThreeOrFourOfAKind deck)
-- checkAllThreeOrFourOfAKind hand= [[Card Heart Ace,Card Diamond Ace,Card Club Ace],[Card Heart Two,Card Spade Two,Card Club Two],[],[],[],[],[],[],[],[],[],[],[]]


convertDeadwoodToPoints:: Meld -> Int
convertDeadwoodToPoints (Deadwood (card)) =toPoints card 
convertDeadwoodToPoints (Set3 _ _ _) = 0
convertDeadwoodToPoints (Set4 _ _ _ _) = 0
convertDeadwoodToPoints (Straight3 _ _ _ ) = 0
convertDeadwoodToPoints (Straight4 _ _ _ _) = 0
convertDeadwoodToPoints (Straight5 _ _ _ _ _) = 0





convertMeldstoPoints:: [Meld]->[Int]
convertMeldstoPoints melds =  map (\meld-> convertDeadwoodToPoints meld) melds

-- GETS ALL DEADWOOD POINTS FROM MELD
totalDeadwoodPoints:: [Meld]->Int
totalDeadwoodPoints l = sum $ convertMeldstoPoints l


toPoints :: Card -> Int
toPoints (Card _ rank) | rank < Jack = fromEnum rank + 1
                       | otherwise = 10 -- Face cards are all worth 10

convertCardstoPoints::[Card] -> [Int]
convertCardstoPoints deck =  map (\card-> toPoints card) deck

getDeadwoodPoints:: [Card]->Int
getDeadwoodPoints l = sum $ convertCardstoPoints l

-- If LESS than 10 points, KNOCK TRUE
checkDeadWoodforKnock:: Int -> Bool
checkDeadWoodforKnock point = if point > 10 then False else True

playerhand1 :: [Card]
playerhand1= [Card Heart Two, Card Heart Three, Card Heart Ace, Card Heart Four, Card Spade Two, Card Spade Three, Card Heart Five, Card Club Eight, Card Club Nine, Card Club Two]

convertHandtoDeadwood:: [Card]->[Meld]
convertHandtoDeadwood deck = map (convertCardtoDeadwood) deck

convertCardtoDeadwood:: Card->Meld
convertCardtoDeadwood (Card s r)  = Deadwood (Card s r) 


-- Get the suit of the card
getSuit :: Card -> Suit
getSuit (Card s _) = s

-- Get the rank of the card
getRank :: Card -> Rank
getRank (Card _ r) = r

-- Getting max card
maxByRank :: [Card] -> Card
maxByRank= foldr1 (\card_a card_b -> if getRank card_a>= getRank card_b then card_a else card_b)

-- Getting min card
minByRank :: [Card] -> Card
minByRank= foldr1 (\card_a card_b -> if getRank card_a <= getRank card_b then card_a else card_b)

groupbyThree:: [Rank]->[[Rank]]
groupbyThree ranks = zipWith const (take 3 <$> tails ranks) (drop (2)ranks)

groupbyFour:: [Rank]->[[Rank]]
groupbyFour ranks = zipWith const (take 4 <$> tails ranks) (drop (3)ranks)

groupbyFive:: [Rank]->[[Rank]]
groupbyFive ranks = zipWith const (take 5 <$> tails ranks) (drop (4)ranks)





-- gets cards of the same suit
cardsSameSuit:: Suit -> [Card] -> [Card]
cardsSameSuit s deck = filter (\card -> getSuit card == s) deck

-- Sort Cards based on suit
-- >>> ranked Heart [Card Heart Three, Card Heart Two, Card Heart Six]
-- [Two,Three,Six]
ranked::  Suit->[Card]->[Rank]
ranked s deck= sort . map getRank $ cardsSameSuit s deck



takeR :: Int -> [a] -> [a]
takeR n = reverse . take n . reverse 

-- mapLists[Ace,Three,Five, Seven, Nine,Ten]
-- mapListsR[Ace,Three,Five, Seven, Nine,Ten]
-- [[Nine],[Seven,Nine],[Five,Seven,Nine],[Three,Five,Seven,Nine],[Ace,Three,Five,Seven,Nine]]
-- ** mapListsR [Two, Three,Four , Five, Nine, Ten, Jack,Queen]
mapListsR ::[Rank] -> [[Rank]]
mapListsR l = map (\a -> takeR a l ) [1..length l]

mapListsL :: [Rank] -> [[Rank]]
mapListsL l = map (\a ->take a l) [1.. length l]

-- listOfRanked Heart [Card Heart Three, Card Heart Two, Card Heart Six,Card Heart Four, Card Heart Five]
-- [[Six],[Three,Six],[Two,Three,Six]]
listOfRanked:: Suit-> [Card] -> [[Rank]]
listOfRanked s deck = rmdups ((mapListsR $ ranked s deck ) ++ (mapListsL $ ranked s deck))
-- listOfRanked Heart [Card Heart Two, Card Heart Three, Card Heart Four, Card Spade Two, Card Spade Three, Card Spade Four,Card Spade Five, Card Diamond Ace, Card Diamond Two, Card Diamond Three]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

longestSequence :: (Ord c, Num c) => (t -> Bool) -> [t] -> c
longestSequence p = maximum . scanl (\count x -> if (p x) then count + 1 else 0) 0



-- Card Heart Eight, Card Heart Two, Card Heart Six,Card Heart Four, Card Heart Five]

-- checkConsecutiveFiveTest Heart [Card Heart Eight, Card Heart Two, Card Heart Six,Card Heart Four, Card Heart Five]
checkConsecutiveThreeTest:: Suit-> [Card] ->[[Card]]
checkConsecutiveThreeTest s d = map (\combo-> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else []) $ (groupbyThree (ranked s d)) 
checkConsecutiveFourTest:: Suit-> [Card] ->[[Card]]
checkConsecutiveFourTest s d = map (\combo-> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else []) $ (groupbyFour (ranked s d)) 
checkConsecutiveFiveTest:: Suit-> [Card] ->[[Card]]
checkConsecutiveFiveTest s d = map (\combo-> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else []) $ (groupbyFive (ranked s d)) 
-- checkRankedConsecutive Club,
-- [[],[],[],[],[],[Ace,Two],[Ace,Two,Three],[]]



checkRankedConsecutive:: Suit -> [Card] -> [[Card]]
checkRankedConsecutive s deck =   map (\combo -> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else [] ) $ listOfRanked s deck

getStraightsNow :: Suit -> [Card] -> [[Card]]
getStraightsNow s deck = ((filter(not.null) $ checkRankedConsecutive s deck))


getDiscardofStraight::[Card]->[Card]
getDiscardofStraight deck = concat (checkRankedConsecutive Heart deck) ++ concat (checkRankedConsecutive Club deck) ++ concat(checkRankedConsecutive Diamond deck) ++ concat (checkRankedConsecutive Spade deck)

--TESTING DISCARD STRAIGHT:: 
-- Gets all discard cards made from Each Suit
getDiscardofStraightTest :: [Card] -> [Card]
getDiscardofStraightTest deck = discardStraightTest Heart deck ++ discardStraightTest Club deck ++ discardStraightTest Spade deck ++ discardStraightTest Diamond deck 


-- ** DONT KNO WHETHER GONNA USE THESE FUNCS
convertRanktoCard:: Suit -> Rank-> Card
convertRanktoCard s r = Card s r


convertSetToCard:: Suit ->[Rank] ->[Card]
convertSetToCard s deck = map(\rank-> convertRanktoCard s rank) deck

-- Takes a list of cards and Forms Melds that are either Straight3, Straight4, or Straight 5
convertStraightMelds::[Card]-> Meld
convertStraightMelds set
    |length set==3 = Straight3 (set!!0) (set!!1) (set!!2)
    |length set==4 = Straight4 (set!!0) (set!!1) (set!!2) (set!!3)
    |otherwise = Straight5 (set!!0) (set!!1)(set!!2)(set!!3)(set!!4)

getAllStraights::Suit -> [Card]->[Meld]
getAllStraights s set = map (convertStraightMelds) straightCombos
    where straightCombos = ((filter(not.null) $ checkRankedConsecutive s set))

-- gets the longest Straight for a Suit for the Player's Hand
-- utilises function ConvertStraightMeld to convert the list of cards into a Straight Meld
-- utilises function checkRankedConsecutive to first get the possible combinations of consecutive cards
-- Once obtained the combination, get the longest sequence to form the Straight Meld
-- If forming Straights with a certain Suit doesn't exist return empty list
getStraightfromSuit::Suit ->[Card]->[Meld]
getStraightfromSuit s set = map (convertStraightMelds) singleStraight
    where 
        straightCombos = ((filter(not.null) $ checkRankedConsecutive s set))
        singleStraight= if straightCombos==[] then [] else [maximumBy (comparing length) straightCombos]

-- Get cards that are Straights so that we can later subtract them from the current Player Hand    
discardStraightTest :: Suit -> [Card] -> [Card]
discardStraightTest s set= if straight ==[] then [] else maximumBy (comparing length) straight
    where 
        straight= ((filter(not.null) $ checkRankedConsecutive s set))

-- MASTER FUNCTION for getting all Straight3, Straight4 and Straight 5 from Player's Hand
-- Concatenates output from getAllStraights from a certain suit together to form master list of straights
-- from each suit
-- getStraightForAllSuit:: [Card]->[Meld]
-- getStraightForAllSuit deck = getAllStraights Heart deck ++getAllStraights Club deck ++getAllStraights Spade deck ++ getAllStraights Diamond deck

getStraightForAllSuit:: [Card]->[Meld]
getStraightForAllSuit deck = getStraightfromSuit Heart deck ++getStraightfromSuit Diamond deck ++getStraightfromSuit Spade deck ++ getStraightfromSuit Club deck

--Returns a list of lists of Ranks that have formed Potential Consecutive Combos of cards
checkPotentialConsecutive:: Suit -> [Card] -> [[Rank]]
checkPotentialConsecutive s deck = map (\combo -> if potentialConsecutiveSet combo then combo else [] ) $ listOfRanked s deck

--Consecutive numbers 
--Must be consecutive else is false
--1,2,3 correct
--1 is wrong 
-- 1,2 is corect

-- Returns True if there are 2 consecutive numbers in the list of Ranks
potentialConsecutiveSet:: [Rank] ->Bool
potentialConsecutiveSet l = ((fromEnum . last) l - (fromEnum . head) l) == (length l - 1) && length l ==2 

-- Returns a boolean value True if the consecutive sequence has a length of three or More
-- Else return False
checkThreeConsecutiveOrMore:: [Rank]->Bool
checkThreeConsecutiveOrMore l = ((fromEnum . last) l - (fromEnum . head) l) == (length l - 1) && length l >2

-- Filters out cards with the same input Rank 
-- Returns List of Cards with Input Rank 
cardsSameRank:: Rank -> [Card] -> [Card]
cardsSameRank r deck = filter (\card -> getRank card == r) deck

-- Making meld of three or four of a kind
-- Return set of 3 or 4 of a kind 
-- Else return empty card list
threeOrFourOfAKind:: Rank -> [Card] -> [Card]
threeOrFourOfAKind r deck = if lengthCardsSameRank r deck > 2 then cardsSameRank r deck else []

-- Master function to map each rank to the function to check if set3 or set4 exists and returns a list
-- of Set3 or Set4 else return empty list if set3 or set4 doesn't exist
-- USAGE:
-- checkAllThreeOrFourOfAKind [Card Club Ace, Card Spade Ace, Card Heart Ace, Card Club Two, Card Spade Two, 
-- Card Diamond Two, Card Heart Queen, Card Club Queen]
-- [[Card Club Ace,Card Spade Ace,Card Heart Ace],[Card Club Two,Card Spade Two,Card Diamond Two],[],[],[],[],[],[],[],[],[],[],[]]
checkAllThreeOrFourOfAKind:: [Card]->[[Card]]
checkAllThreeOrFourOfAKind deck = map (\rank -> threeOrFourOfAKind rank deck) [Ace , Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten
          , Jack , Queen , King]
          
-- Master function that will form All Set3/Set4 
-- Obtains all non-empty combos that can form melds Set3 and Set4
-- Map over the combos and form Melds using formSetMeld Function
formAllSetMeld:: [Card]->[Meld]
formAllSetMeld deck = map (formSetMeld) (filter(not.null) $ checkAllThreeOrFourOfAKind deck)

--Pattern matches accordingly to length of card list 
--If Length = 3 then make a Set3
--If Length = 4 then make a Set4
--Otherwise it will just make a Deadwood Meld
formSetMeld:: [Card] -> Meld 
formSetMeld set 
    |length set==3 = Set3 (set!!0) (set!!1) (set!!2)
    |length set==4 = Set4 (set!!0) (set!!1) (set!!2) (set!!3)
    |otherwise =Deadwood (set!!0)

--helper function to check for number of cards with same rank
lengthCardsSameRank:: Rank -> [Card] -> Int
lengthCardsSameRank r deck = length $ cardsSameRank r deck

--check GIN meaning can fit all 10 into meld




--discard card that is useless