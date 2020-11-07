-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where
import Parser.Instances
import Parser.Parser ( character, is, unexpectedCharParser, (|||) ) -- This is the source for the parser from the course notes
import Rummy.Types
    ( Act(Drop, Gin, Knock),
      Action(Action),
      ActionFunc,
      Draw(Stock,Discard),
      Meld(..),
      MeldFunc,
      PlayFunc,Score )   -- Here you will find types used in the game of Rummy
import Cards
    ( Card(..),
      Rank(Jack, Ace, King, Two, Three,Four, Five,Six,Seven, Eight, Nine,Queen,Ten),
      Suit(Club, Diamond, Spade, Heart) )         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import EitherIO
import Data.List
import Data.Ord
import Data.Functor

-- import Debug.Trace

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
-- Procedure:
-- if Top Card + two of a kind = Three of a Kind -> then take discard
-- if Top Card + three of a kind = Four of a kind -> then take discard
-- if Top Card + consecutive Two = Straight 3 -> take discard
-- if Top Card + consecutive Three/consecutive Four = Straight 4/ Straight 5 -> take discard
-- otherwise if top card is not meldable with any set or straight then just take from Stock

pickCard :: ActionFunc
pickCard topDiscard score memory _ hand  
    | or(checkTopCardThreeSet topDiscard hand)==True= (Discard, convertedmemory)
    | or (checkTopCardTwoSet topDiscard hand)==True= (Discard, convertedmemory)
    | or (checkTopCardConsecutiveTwo topDiscard hand)==True = (Discard,convertedmemory)
    | or (checkTopCardConsecutiveThreeFour topDiscard hand)==True= (Discard,convertedmemory)
    | otherwise= (Stock,convertedmemory)
    where convertedmemory= justToStr memory ++ show score 

-- Helper function to convert Just Output into a String 
justToStr::Maybe String -> String
justToStr (Just a)= a
justToStr (Nothing)= ""

-- Function taken from week 11 tut to form Parser String
string :: String -> Parser String
string = traverse is

enemyStock :: Parser Draw
enemyStock = string "sto" >> pure Stock
enemyDiscard :: Parser Draw
enemyDiscard =string "dis" >> pure Discard
choiceEnemy :: Parser Draw
choiceEnemy = enemyStock||| enemyDiscard 



-- Function taken from week 11 tut in order to get list of items from memory 
list :: Parser a -> Parser [a]
list p = list1 p ||| pure []
list1 :: Parser a -> Parser [a]
list1 p = do
  first <- p
  rest <- list p
  pure (first:rest)

-- function taken from week 11 tut
spaces :: Parser ()
spaces = (is ' ' >> spaces) ||| pure ()

doublescores :: Parser [Char]
doublescores = do
  spaces
  _ <- is '('
  a <- doubleDigit
  _ <- is ','
  b <- doubleDigit
  _ <- is ')'
  pure (a++b)

singleScore ::Parser[Char]
singleScore = do
    spaces
    _ <- is '('
    a <-singleDigit
    _ <- is ','
    b <- singleDigit
    _ <- is ')'
    pure ("0"++a++"0"++b)

-- function taken from week 11 tutorial
thisMany :: Int -> Parser a -> Parser [a]
thisMany n p = sequence (replicate n p)

singleDigit :: Parser [Char]
singleDigit = thisMany 1 digit


doubleDigit :: Parser [Char]
doubleDigit= thisMany 2 digit

singleDoubleScore:: Parser [Char]
singleDoubleScore = do
    spaces 
    _ <- is '('
    a <-singleDigit
    _ <- is ','
    b <-doubleDigit
    _ <- is ')'
    pure ("0"++a++b)

doubleSingleScore ::Parser[Char]
doubleSingleScore = do
    spaces
    _ <- is '('
    a <-doubleDigit
    _ <- is ','
    b <- singleDigit
    _ <- is ')'
    pure (a++"0"++b)

-- Master parser combinator to parse 
-- EG: (0,0), (0,40), (30,0) (30,30)
allscores:: Parser[Char]
allscores = singleScore|||singleDoubleScore|||doubleSingleScore|||doublescores

-- Parser inspired from course notes to form digits
digit :: Parser Char
digit = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

-- Parsers to check drop, knock, gin
dropChar ::Parser [Char]
dropChar = string "drop" >> pure "drop"
knockChar ::Parser [Char]
knockChar = string "knock" >> pure "knock"
ginChar ::Parser [Char]
ginChar = string "gin" >> pure "gin"

-- Master Parser Combinator to check if Gin, Knock , Drop has occurred
playerAction :: Parser [Char]
playerAction = ginChar||| knockChar |||dropChar

-- Parser Combinator for All Memory should consist of scores and action taken 
allMemory :: Parser[[Char]]
allMemory = do
    spaces
    a<-allscores
    b<-playerAction
    pure ([a,b])

-- Function inspired from course notes on obtaining Result from Parser
getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "You should not do that!"

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

-- For this function, I have parsed my memory containing the list of actions taken from the player and the previous round's score
-- If the score has changed from previous round and the previous round action was drop then, that means that the player must drop card
-- If score has not changed and previosu round action was drop , then can Knock Or Gin
-- Procedure
-- 1. Check if after dropping highest card that all melds are formed ,can call Gin 
-- 2. Check if after forming melds with all 11 cards, there is only 1 deadwood card left (this card cannot be same as picked card) 
--  means can call Gin
-- 3. check if among the deadwood, there are total points of deadwood <10 , can Knock
-- 4. Drop Highest deadwood card

playCard :: PlayFunc
-- playCard _ _ _ deck = ( Action Drop a ,"lol") where a = deck!!0
playCard pickedCard score memory deck 
    -- =trace ("input deck:"++ show deck ++"picked card:" ++show pickedCard)(Action Drop (highestcard) ,"lol")
    |all (isNotDeadwood)(makeMelds (30,30)"lol" deckafterdropcard)==True && playerscoreChanged==False && previousRoundaction=="drop"  = (Action Gin highestcard,(memory++"gin"))
    |length deadwoodMelds==1 && playerscoreChanged==False && previousRoundaction=="drop"  = (Action Gin discarded,(memory++"gin"))
    |finalPoints<=10 && discarded/=pickedCard && playerscoreChanged==False && previousRoundaction=="drop"   = (Action Knock discarded,(memory++"knock"))
    |otherwise = (Action Drop discarded,(memory++"drop"))    
    where 
        highestcard= last (sort (deck))
        deckafterdropcard= (deck\\[highestcard]) ++[pickedCard]
        meldsFormed=(makeMelds (30,30) "lol" deckPlusNewCard)
        deckPlusNewCard= deck++[pickedCard]
        deadwoodMelds= filter(isDeadwood) meldsFormed
        deadwoodCardDiscard = (concat( (convertMeldtoCard) <$> deadwoodMelds)) 
        discarded=if length deadwoodCardDiscard >=1 && checkIfMaxsamePick==False then maxByRank deadwoodCardDiscard else highestcard
        checkIfMaxsamePick= if maxByRank deadwoodCardDiscard==pickedCard then True else False
        deckwithoutDiscard= deckPlusNewCard\\ [discarded]
        newMeldsformed= (makeMelds (30,30) "lol" deckwithoutDiscard)
        finalDeadwood= filter(isDeadwood) newMeldsformed
        finalPoints= totalDeadwoodPoints finalDeadwood
        actionlist =  (getMem (parse (list allMemory) memory))
        checkfirstRound= if length actionlist==0 then True else False
        previousRoundaction = if checkfirstRound== True then "firstround" else last(last(actionlist))
        previousRoundScore= if checkfirstRound== True then "firstround" else head(last(actionlist))
        playerscoreChanged= if previousRoundScore/="firstround" then checkScoreChanged score previousRoundScore else False

-- Helper functions to convert the Strings into Integer
-- EG: Score will be "3050"
-- p1 Score would return 30 in integer form
-- p2 Score will return 50 in integer form 
p1 :: String-> Int
p1 chars = read (take 2 chars)::Int
p2 :: String-> Int
p2 chars = read (take 2 (drop 2 chars))::Int

-- Input : Current Score , PreviousScore
-- Function will convert the PreviousScore in string form to an Integer 
-- and will compare the previousscore to the current score
-- If any change occur then return True
-- Else return False
checkScoreChanged :: (Score,Score)-> String -> Bool
checkScoreChanged curscore prvscore 
    |  p1 prvscore /= fst curscore = True
    |  p2 prvscore /= snd curscore = True
    | otherwise = False

-- Get the rank of the card
getRankMeld :: Meld-> Rank
getRankMeld (Deadwood (Card _ r)) = r
getRankMeld (_)=Ace

-- Getting max card
-- Will be used later to get the highest rank deadwoodcard in your hand
maxByRankMeld :: [Meld] -> Meld
maxByRankMeld= foldr1 (\meld_a meld_b -> if getRankMeld meld_a>= getRankMeld meld_b then meld_a else meld_b)

-- Helper function to convert the Deadwood Melds to Cards
convertMeldtoCard:: Meld -> [Card]
convertMeldtoCard (Deadwood (Card s r)) = [(Card s r)]
convertMeldtoCard (Set3 _ _ _) = []
convertMeldtoCard (Set4 _ _ _ _) = []
convertMeldtoCard (Straight3 _ _ _) = []
convertMeldtoCard (Straight4 _ _ _ _) = []
convertMeldtoCard (Straight5 _ _ _ _ _) = []

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
-- Which melds to use for scoring.
-- type MeldFunc
--   = (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> String        -- ^ the player's memory
--   -> [Card]        -- ^ cards in player's hand
--   -> [Meld]        -- ^ elected melds

-- The way I have formed my makeMelds function is by using guards
-- Procedure
-- 1. Check if there are more Set3 Set4 Melds compared to Straight Melds and form the Set Melds first
-- then only form straights with the remaiing cards, lastly form Deadwood with the remaining cards.
-- 2. Check if there are more Straights than Sets and form Straight Melds and with the remaining cards 
-- form Straight Melds
-- 3. Check if there are equal amount of Sets and Straights, if yes then form Sets first before Straights
-- 4. If there are 0 sets and 0 straights, directly form all deadwood
makeMelds :: MeldFunc
makeMelds _ _ deck 
    | length (formAllSetMeld deck) > length(getStraightForAllSuit deck)  = formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
    | length (getStraightForAllSuit deck)> length (formAllSetMeld deck) = getStraightForAllSuit deck ++ formAllSetMeld discardStraightNoSet ++ convertHandtoDeadwood finaldeadwood
    | length(formAllSetMeld deck)== length (getStraightForAllSuit deck )= formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
    | length (getStraightForAllSuit deck)==0  && length(formAllSetMeld deck)== 0 = convertHandtoDeadwood deck
    | otherwise = convertHandtoDeadwood deck
    where 
        remainder = deck \\ remainderCards deck
        discardStraight = remainder \\ getDiscardofStraight remainder
        discardStraightNoSet = deck\\getDiscardofStraight deck
        finaldeadwood= discardStraightNoSet \\ remainderCards discardStraightNoSet

-- Takes meld as input and checks if its not a Deadwood
isNotDeadwood :: Meld -> Bool
isNotDeadwood (Deadwood _) = False
isNotDeadwood _ = True

-- Takes meld as input and checks if it is Deadwood
isDeadwood :: Meld -> Bool
isDeadwood (Deadwood _) = True
isDeadwood _ = False

-- Helper function to get list of cards that form set
-- So that later on I can subtract the player's hand with these cards that have formed sets
-- EG: inputdeck = [Card Club Two, Card Heart Two, Card Spade Two, Card Heart Ace, Card Club Ace, Card Spade Ace, Card Club Five, Card Club Seven , Card Club Nine,Card Club Ten]
remainderCards :: [Card]-> [Card]
remainderCards deck= concat (filter(not.null) $ checkAllThreeOrFourOfAKind deck)

-- Helper function to easily convert Deadwood meld into Points
convertDeadwoodToPoints:: Meld -> Int
convertDeadwoodToPoints (Deadwood (card)) =toPoints card 
convertDeadwoodToPoints (Set3 _ _ _) = 0
convertDeadwoodToPoints (Set4 _ _ _ _) = 0
convertDeadwoodToPoints (Straight3 _ _ _ ) = 0
convertDeadwoodToPoints (Straight4 _ _ _ _) = 0
convertDeadwoodToPoints (Straight5 _ _ _ _ _) = 0

-- Convert list of Deadwood Melds to Points
convertMeldstoPoints:: [Meld]->[Int]
convertMeldstoPoints melds = convertDeadwoodToPoints <$> melds

-- GETS ALL DEADWOOD POINTS FROM MELD
totalDeadwoodPoints:: [Meld]->Int
totalDeadwoodPoints l = sum $ convertMeldstoPoints l

-- convert individual card to points
toPoints :: Card -> Int
toPoints (Card _ rank) | rank < Jack = fromEnum rank + 1
                       | otherwise = 10 -- Face cards are all worth 10

-- convert cards into list of points
convertCardstoPoints::[Card] -> [Int]
convertCardstoPoints deck =  toPoints <$> deck

-- Convert list of Deadwood cards into points and Sum them up
-- Outputs final Deadwood Points
getDeadwoodPoints:: [Card]->Int
getDeadwoodPoints l = sum $ convertCardstoPoints l

-- Master function to convert a list of cards into deadwood
convertHandtoDeadwood:: [Card]->[Meld]
convertHandtoDeadwood deck =  (convertCardtoDeadwood) <$> deck

-- Take Card and convert it to Deadwood
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

-- gets cards of the same suit
cardsSameSuit:: Suit -> [Card] -> [Card]
cardsSameSuit s  = filter ((s ==) . getSuit)

-- Sort Cards based on suit
-- Function that returns a sorted list of ranks based on the Suit Provided
ranked::  Suit->[Card]->[Rank]
ranked s deck= sort . map getRank $ cardsSameSuit s deck

-- Function to help get the last n items in a list
takeR :: Int -> [a] -> [a]
takeR n = reverse . take n . reverse 

-- Recursively remove head of the list
mapListsR ::[Rank] -> [[Rank]]
mapListsR l =  (\a -> takeR a l ) <$> [1..length l]

-- Recursively remove the tail of the list
mapListsL :: [Rank] -> [[Rank]]
mapListsL l =  (\a ->take a l) <$> [1.. length l]

-- USAGE:
-- listOfRanked Heart [Card Heart Three, Card Heart Two, Card Heart Six,Card Heart Four, Card Heart Five]
-- Function that will create a list of list of ranks by taking the output 
-- of recursively removing the head of the list 
-- concatenated with the output of recursively removing the tail of list 
-- to get a final list of list of ranks that consist of multiple groupings of rank
listOfRanked:: Suit-> [Card] -> [[Rank]]
listOfRanked s deck = rmdups ((mapListsR $ ranked s deck ) ++ (mapListsL $ ranked s deck))

-- Function to help remove duplicates
-- Inspired from StackOverflow Post (Removing duplicates from a list in Haskell)
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Function that outpus groups of consecutive Cards that are length 3 or more 
checkRankedConsecutive:: Suit -> [Card] -> [[Card]]
checkRankedConsecutive s deck =  filter(not.null) $ map (\combo -> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else [] ) $ listOfRanked s deck

-- Function that returns a list of list  of ranks that have passed the consecutive function
-- EG: checkthreefour Heart [Card Heart Ace, Card Heart Two, Card Heart Three, Card Heart Four]
-- Output : [[Ace,Two,Three],[Ace,Two,Three,Four],[Two,Three,Four]]
checkthreefour::Suit->[Card]->[[Rank]]
checkthreefour s deck=filter(not.null) $ map (\combo -> if checkThreeConsecutiveOrMore combo then  combo else [] ) $ listOfRanked s deck

--TESTING DISCARD STRAIGHT:: 
-- Gets all discard cards made from Each Suit
-- Returns list of cards that were not able to form Straights
getDiscardofStraight :: [Card] -> [Card]
getDiscardofStraight deck = concat (flip discardStraightTest deck <$> [Spade .. ])

-- Input: Suit , Rank
-- Output Card
convertRanktoCard:: Suit -> Rank-> Card
convertRanktoCard s r = Card s r

-- takes the suit and the list of ranks to return back the deck of cards
convertSetToCard:: Suit ->[Rank] ->[Card]
convertSetToCard s deck = convertRanktoCard s <$> deck

-- Takes a list of cards and Forms Melds that are either Straight3, Straight4, or Straight 5
convertStraightMelds::[Card]-> [Meld]
convertStraightMelds set
    |length set==3 = [Straight3 (set!!0) (set!!1) (set!!2)]
    |length set==4 = [Straight4 (set!!0) (set!!1) (set!!2) (set!!3)]
    |length set==5 =[Straight5 (set!!0) (set!!1)(set!!2)(set!!3)(set!!4)]
    |length set==6 =[Straight3 (set!!0) (set!!1) (set!!2), Straight3(set!!3)(set!!4)(set!!5)]
    |length set==7 =[Straight4 (set!!0)(set!!1)(set!!2)(set!!3), Straight3(set!!4)(set!!5)(set!!6) ]
    |length set==8 =[Straight4 (set!!0)(set!!1) (set!!2)(set!!3), Straight4(set!!4)(set!!5)(set!!6)(set!!7) ]
    |length set==9 =[Straight5 (set!!0)(set!!1) (set!!2)(set!!3)(set!!4), Straight4(set!!5)(set!!6)(set!!7)(set!!8) ]
    |length set==10 =[Straight5 (set!!0)(set!!1) (set!!2)(set!!3)(set!!4), Straight5(set!!5)(set!!6)(set!!7)(set!!8)(set!!9) ]
    | otherwise = []

-- Master function that returns all of the different Straights that can be formed 
-- after obtaining a group of sorted cards that have been checked whether they are consecutive ranks
getAllStraights::Suit -> [Card]->[Meld]
getAllStraights s set = concat ( (convertStraightMelds) <$> straightCombos)
    where straightCombos = ((filter(not.null) $ checkRankedConsecutive s set))

-- gets the longest Straight for a Suit for the Player's Hand
-- utilises function ConvertStraightMeld to convert the list of cards into a Straight Meld
-- utilises function checkRankedConsecutive to first get the possible combinations of consecutive cards
-- Once obtained the combination, get the longest sequence to form the Straight Meld
-- If forming Straights with a certain Suit doesn't exist return empty list
getStraightfromSuit::Suit ->[Card]->[Meld]
getStraightfromSuit s set = concat(  (convertStraightMelds) <$> singleStraight)
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
getStraightForAllSuit deck = concat (flip getStraightfromSuit deck <$> [Spade .. ])

--Returns a list of lists of Ranks that have formed Potential Consecutive Combos of cards
checkPotentialConsecutive:: Suit -> [Card] -> [[Rank]]
checkPotentialConsecutive s deck = filter(not.null) $ (\combo -> if potentialConsecutiveSet combo then combo else [] ) <$> listOfRanked s deck

-- just need to the one with getSuit Card Top
checkTopCardConsecutiveTwo :: Card -> [Card] -> [Bool]
checkTopCardConsecutiveTwo topcard deck= (\combo-> if checkThreeConsecutiveOrMore(sort(combo ++[getRank topcard]))==True then True else False) <$> checkPotentialConsecutive (getSuit topcard) (deck)

-- Master function to check if the top card fits into any straight of our hand
-- to form Straigh4, Straight5
checkTopCardConsecutiveThreeFour :: Card->[Card]->[Bool]
checkTopCardConsecutiveThreeFour topcard deck = (\combo-> if checkThreeConsecutiveOrMore(sort(combo ++[getRank topcard]))==True then True else False) <$> checkthreefour (getSuit topcard) (deck)

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
cardsSameRank r deck = filter ((r ==) . getRank) deck

-- Making meld of three or four of a kind
-- Return set of 3 or 4 of a kind 
-- Else return empty card list
threeOrFourOfAKind:: Rank -> [Card] -> [Card]
threeOrFourOfAKind r deck = if lengthCardsSameRank r deck > 2 then cardsSameRank r deck else []

-- helper function that will check for input rank 
-- whether there are cards of the same rank that is length 2
twoOfAKind :: Rank -> [Card] -> [Card]
twoOfAKind r deck = if lengthCardsSameRank r deck == 2  then cardsSameRank r deck else []

-- Returns list of cards that are two of a kind
-- eg: Card Heart Ace, Card Spade Ace
checkAllTwoofAKind :: [Card] -> [[Card]]
checkAllTwoofAKind deck = filter (not . null) (map (flip twoOfAKind deck) [Ace .. ])

-- Input: Top Card
-- Check if top card is meldable to form Set3 with any cards that form 2 of a kind in my hand
checkTopCardTwoSet :: Card -> [Card] -> [Bool]
checkTopCardTwoSet card deck = (\combo->if getRank (head combo)==getRank card then True else False) <$> checkAllTwoofAKind deck

-- Input : Top Card
-- Output a list of boolean for each group of card that is meldable to form Set4
checkTopCardThreeSet ::Card-> [Card]->[Bool]
checkTopCardThreeSet card deck = (\combo->if length combo ==3 && getRank (head combo)==getRank card then True else False) <$> checkAllThreeOrFourOfAKind deck

-- Master function to map each rank to the function to check if set3 or set4 exists and returns a list
-- of Set3 or Set4 else return empty list if set3 or set4 doesn't exist
-- USAGE:
-- checkAllThreeOrFourOfAKind [Card Club Ace, Card Spade Ace, Card Heart Ace, Card Club Two, Card Spade Two, 
-- Card Diamond Two, Card Heart Queen, Card Club Queen]
-- [[Card Club Ace,Card Spade Ace,Card Heart Ace],[Card Club Two,Card Spade Two,Card Diamond Two],[],[],[],[],[],[],[],[],[],[],[]]
checkAllThreeOrFourOfAKind:: [Card]->[[Card]]
checkAllThreeOrFourOfAKind deck = filter(not.null) $ map (\rank -> threeOrFourOfAKind rank deck) [Ace .. ]
          
-- Master function that will form All Set3/Set4 
-- Obtains all non-empty combos that can form melds Set3 and Set4
-- Map over the combos and form Melds using formSetMeld Function
formAllSetMeld:: [Card]->[Meld]
formAllSetMeld deck = map (formSetMeld) $ checkAllThreeOrFourOfAKind deck

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






