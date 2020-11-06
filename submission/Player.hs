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
import Control.Monad
import Rummy.Play

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
pickCard :: ActionFunc
pickCard topDiscard score memory _ hand  
    | or(checkTopCardThreeSet topDiscard hand)==True= (Discard, convertedmemory)
    | or (checkTopCardThreeSet topDiscard hand)==True= (Discard,convertedmemory)
    | or (checkTopCardConsecutiveTwo topDiscard hand)==True = (Discard,convertedmemory)
    | or (checkTopCardConsecutiveThreeFour topDiscard hand)==True= (Discard,convertedmemory)
    | otherwise= (Stock,convertedmemory)
    where convertedmemory= justToStr memory ++ show score 

justToStr::Maybe String -> String
justToStr (Just a)= a
justToStr (Nothing)= ""

string :: String -> Parser String
string = traverse is

-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
enemyStock :: Parser Draw
enemyStock = string "sto" >> pure Stock
enemyDiscard :: Parser Draw
enemyDiscard =string "dis" >> pure Discard
choiceEnemy :: Parser Draw
choiceEnemy = enemyStock||| enemyDiscard 

calledGin:: Parser Act 
calledGin = string "gin">> pure Gin
calledKnock:: Parser Act 
calledKnock = string "knock">> pure Knock
calledDrop:: Parser Act 
calledDrop = string "drop">> pure Drop



-- from list of actions check if the last action was knock or gin 
actionPlayer::Parser Act
actionPlayer = calledGin||| calledKnock ||| calledDrop

-- dropCard:: Parser Card
-- dropCard = string (Card _ _)>> pure Card

list :: Parser a -> Parser [a]
list p = list1 p ||| pure []
list1 :: Parser a -> Parser [a]
--list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))
list1 p = do
  first <- p
  rest <- list p
  pure (first:rest)


-- discardTop:: Parser Card
-- discardTop 

-- | Play a round of RPS given the result of the previous round.

-- data Animal = Cat | Dog | Camel
--   deriving Show


-- cat,dog, camel :: Parser Animal
-- dog = string "dog" >> pure Dog
-- camel = string "camel" >> pure Camel
-- cat = string "cat" >> pure Cat
-- animal :: Parser Animal
-- animal = cat ||| dog ||| camel
-- | Associative container type.




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

allscores:: Parser[Char]
allscores = singleScore|||singleDoubleScore|||doubleSingleScore|||doublescores

digit :: Parser Char
digit = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

dropChar ::Parser [Char]
dropChar = string "drop" >> pure "drop"
knockChar ::Parser [Char]
knockChar = string "knock" >> pure "knock"
ginChar ::Parser [Char]
ginChar = string "gin" >> pure "gin"


playerAction :: Parser [Char]
playerAction = ginChar||| knockChar |||dropChar

allMemory :: Parser[[Char]]
allMemory = do
    spaces
    a<-allscores
    b<-playerAction
    pure ([a,b])



getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "You should not do that!"

--  master function for meldable
-- if discard + set2= Set3
-- if discard + set3= Set4
-- if discard + straight 2 = Straight3
-- if discard + straight3= straight 4
-- if discard + straight4=straight 5

-- if any possbile then take discard
-- else take stock
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
        deadwoodCardDiscard = (concat(map (convertMeldtoCard) deadwoodMelds)) 

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


p1 :: String-> Int
p1 chars = read (take 2 chars)::Int
p2 :: String-> Int
p2 chars = read (take 2 (drop 2 chars))::Int

checkScoreChanged :: (Score,Score)-> String -> Bool
checkScoreChanged curscore prvscore 
    |  p1 prvscore /= fst curscore = True
    |  p2 prvscore /= snd curscore = True
    | otherwise = False


myIntToStr :: Int -> String
myIntToStr x
    | x < 3     = show x ++ " is less than three"
    | otherwise = "normal"
--check if can form gin with current hand +
-- 

-- Get the rank of the card
getRankMeld :: Meld-> Rank
getRankMeld (Deadwood (Card _ r)) = r
getRankMeld (_)=Ace

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

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
-- Which melds to use for scoring.
-- type MeldFunc
--   = (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> String        -- ^ the player's memory
--   -> [Card]        -- ^ cards in player's hand
--   -> [Meld]        -- ^ elected melds
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

-- Player hand = [Card Club Two, Card Heart Two, Card Spade Two, Card Heart Ace, Card Club Ace, Card Spade Ace, Card Club Five, Card Club Seven , Card Club Nine,Card Club Ten]
remainderCards :: [Card]-> [Card]
remainderCards deck= concat (filter(not.null) $ checkAllThreeOrFourOfAKind deck)

convertDeadwoodToPoints:: Meld -> Int
convertDeadwoodToPoints (Deadwood (card)) =toPoints card 
convertDeadwoodToPoints (Set3 _ _ _) = 0
convertDeadwoodToPoints (Set4 _ _ _ _) = 0
convertDeadwoodToPoints (Straight3 _ _ _ ) = 0
convertDeadwoodToPoints (Straight4 _ _ _ _) = 0
convertDeadwoodToPoints (Straight5 _ _ _ _ _) = 0

convertMeldstoPoints:: [Meld]->[Int]
convertMeldstoPoints melds =   (\meld-> convertDeadwoodToPoints meld) <$> melds

-- GETS ALL DEADWOOD POINTS FROM MELD
totalDeadwoodPoints:: [Meld]->Int
totalDeadwoodPoints l = sum $ convertMeldstoPoints l


toPoints :: Card -> Int
toPoints (Card _ rank) | rank < Jack = fromEnum rank + 1
                       | otherwise = 10 -- Face cards are all worth 10

convertCardstoPoints::[Card] -> [Int]
convertCardstoPoints deck =   (\card-> toPoints card) <$> deck

getDeadwoodPoints:: [Card]->Int
getDeadwoodPoints l = sum $ convertCardstoPoints l

-- If LESS than 10 points, KNOCK TRUE
checkDeadWoodforKnock:: Int -> Bool
checkDeadWoodforKnock point = if point > 10 then False else True

playerhand1 :: [Card]
playerhand1= [Card Heart Two, Card Heart Three, Card Heart Ace, Card Heart Four, Card Spade Two, Card Spade Three, Card Heart Five, Card Club Eight, Card Club Nine, Card Club Two]

convertHandtoDeadwood:: [Card]->[Meld]
convertHandtoDeadwood deck =  (convertCardtoDeadwood) <$> deck

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
cardsSameSuit s  = filter (\card -> getSuit card == s) 

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
longestSequence p = maximum . scanl (\num x -> if (p x) then num + 1 else 0) 0

checkRankedConsecutive:: Suit -> [Card] -> [[Card]]
checkRankedConsecutive s deck =  filter(not.null) $ map (\combo -> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else [] ) $ listOfRanked s deck

checkthreefour::Suit->[Card]->[[Rank]]
checkthreefour s deck=filter(not.null) $ map (\combo -> if checkThreeConsecutiveOrMore combo then  combo else [] ) $ listOfRanked s deck

getStraightsNow :: Suit -> [Card] -> [[Card]]
getStraightsNow s deck =   checkRankedConsecutive s deck


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


getAllStraights::Suit -> [Card]->[Meld]
getAllStraights s set = concat (map (convertStraightMelds) straightCombos)
    where straightCombos = ((filter(not.null) $ checkRankedConsecutive s set))

-- gets the longest Straight for a Suit for the Player's Hand
-- utilises function ConvertStraightMeld to convert the list of cards into a Straight Meld
-- utilises function checkRankedConsecutive to first get the possible combinations of consecutive cards
-- Once obtained the combination, get the longest sequence to form the Straight Meld
-- If forming Straights with a certain Suit doesn't exist return empty list
getStraightfromSuit::Suit ->[Card]->[Meld]
getStraightfromSuit s set = concat( map (convertStraightMelds) singleStraight)
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
checkPotentialConsecutive s deck = filter(not.null) $ (\combo -> if potentialConsecutiveSet combo then combo else [] ) <$> listOfRanked s deck

-- just need to the one with getSuit Card Top
checkTopCardConsecutiveTwo :: Card -> [Card] -> [Bool]
checkTopCardConsecutiveTwo topcard deck= (\combo-> if checkThreeConsecutiveOrMore(sort(combo ++[getRank topcard]))==True then True else False) <$> checkPotentialConsecutive (getSuit topcard) (deck)

checkTopCardConsecutiveThreeFour :: Card->[Card]->[Bool]
checkTopCardConsecutiveThreeFour topcard deck = (\combo-> if checkThreeConsecutiveOrMore(sort(combo ++[getRank topcard]))==True then True else False) <$> checkthreefour (getSuit topcard) (deck)

-- getStraight2forAllSuit :: [Card]->[[Rank]]
-- getStraight2forAllSuit deck= checkPotentialConsecutive Heart deck ++checkPotentialConsecutive Spade deck++checkPotentialConsecutive Club deck++checkPotentialConsecutive Diamond deck
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


twoOfAKind :: Rank -> [Card] -> [Card]
twoOfAKind r deck = if lengthCardsSameRank r deck == 2  then cardsSameRank r deck else []

checkAllTwoofAKind :: [Card] -> [[Card]]
checkAllTwoofAKind deck = filter(not.null)$ map(\rank->twoOfAKind rank deck)[Ace , Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten
          , Jack , Queen , King] 


checkTopCardTwoSet :: Card -> [Card] -> [Bool]
checkTopCardTwoSet card deck = (\combo->if getRank (head combo)==getRank card then True else False) <$> checkAllTwoofAKind deck

checkTopCardThreeSet ::Card-> [Card]->[Bool]
checkTopCardThreeSet card deck = (\combo->if length combo ==3 && getRank (head combo)==getRank card then True else False) <$> checkAllThreeOrFourOfAKind deck

-- Master function to map each rank to the function to check if set3 or set4 exists and returns a list
-- of Set3 or Set4 else return empty list if set3 or set4 doesn't exist
-- USAGE:
-- checkAllThreeOrFourOfAKind [Card Club Ace, Card Spade Ace, Card Heart Ace, Card Club Two, Card Spade Two, 
-- Card Diamond Two, Card Heart Queen, Card Club Queen]
-- [[Card Club Ace,Card Spade Ace,Card Heart Ace],[Card Club Two,Card Spade Two,Card Diamond Two],[],[],[],[],[],[],[],[],[],[],[]]
checkAllThreeOrFourOfAKind:: [Card]->[[Card]]
checkAllThreeOrFourOfAKind deck = filter(not.null) $ map (\rank -> threeOrFourOfAKind rank deck) [Ace , Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten
          , Jack , Queen , King]
          
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






