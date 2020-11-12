# FIT2102 Gin Rummy Report

In this report, I will be discussing the process of how I have come up with my final code, strategy and each function's functionalities.

# Strategy 🃏

## **Pick Card Function**

My strategy includes picking cards from the visible discard that are "Meldable" meaning that they help to form any Set within my hand. By building the makeMeld function first I was able to make functions to form Potential Sets and Potential Consecutive sets easily due to the similarity of these functions. In terms of memory passing, I will be passing the score of the current round to the playCard function in my new Memory. 

- TopCard + Any Two of A Kind = Meldable
- TopCard + Any Three of A Kind = Meldable
- TopCard + Any Two Consecutive = Meldable
- TopCard + Any Three Consecutive = Meldable
- TopCard + Any Four Consecutive = Meldable
- If the TopCard is not Meldable then choose the Stock Pile.

## **Play Card Function**

I have a few heuristic procedures in order to decide whether Gin, Knock or Drop should be called. Throughout the playCard function, I have implemented a check on the Memory so that I can check whether the score has changed and whether the last action has been a Knock or Gin. The output memory will be the previous memory concatenated with the current action being taken. 

1. I first check if after discarding the highest card from my hand and then adding the pickedCard to my hand whether **No Deadwood** has been formed after calling the makeMelds function. If no deadwood is formed, that means that I can **call Gin** as all cards have been grouped into Melds. 
2. If the above is not true, then I will use the MakeMelds function on the **Input Deck + pickedCard.** From here if there is only one Deadwood left, I can immediately **call Gin** as that means that **Ten out of Eleven Cards** are able to form Sets.
3. Lastly, I check by calling the makeMelds function on all eleven cards and discard the highest ranking deadwood card. If by summing all deadwood points it is <= 10 , and that the discarded card is not the same as the pickedCard , I would be able to call Knock.
4. Otherwise, I should just drop the highest ranking deadwood card in the player's hand.

## Make Meld Function

For the makeMeld function, I have used an approach whereby I look at whether I have more Sets or Straights and form those Melds first before forming the latter. I then check if there are any remaining cards and form Deadwood Melds for them. 

## Process of building my code

1. **Building the makeMelds Function**
    1. Build small functions such as getSuit and getRank in order to easily obtain Suit and Rank from cards. **Pattern Matches** the Card Constructor to get the Suit and the Rank

        ```haskell
        -- Get the suit of the card
        getSuit :: Card -> Suit
        getSuit (Card s _) = s

        -- Get the rank of the card
        getRank :: Card -> Rank
        getRank (Card _ r) = r
        ```

    2. For the process of building a function that will be able to form Straight3, Straight4 and Straight5 Melds. I would need to have the ability to form groups of cards with the same suit and from there have the ability to sort them base on them their ranks. I used **Filter** to obtain cards that have the same rank and **Map** to obtain the ranks of cards from the same suit.

        ```haskell
        -- Filters out cards with the same input Rank 
        -- Returns List of Cards with Input Rank 
        cardsSameRank:: Rank -> [Card] -> [Card]
        cardsSameRank r deck = filter ((r ==) . getRank) deck

        -- Sort Cards based on suit
        -- >>> ranked Heart [Card Heart Three, Card Heart Two, Card Heart Six]
        -- [Two,Three,Six]
        ranked::  Suit->[Card]->[Rank]
        ranked s deck= sort . map getRank $ cardsSameSuit s deck
        ```

    3. Form All Set Melds by mapping each group of cards to their respective Melds which is either Set3 or Set4. It is broken down into four separate functions whereby, I first check if the length is bigger than three, and then I have another function to loop through the group of cards and check if the length is bigger than three, and then finally I form Set Melds with those group of cards that have fit the Set3/Set4 conditions.

        ```haskell
        -- Making meld of three or four of a kind
        -- Return set of 3 or 4 of a kind 
        -- Else return empty card list
        threeOrFourOfAKind:: Rank -> [Card] -> [Card]
        threeOrFourOfAKind r deck = if lengthCardsSameRank r deck > 2 then cardsSameRank r deck else []

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
        ```

    4. I formed combinations of consecutive cards using these functions below.

        ```haskell
        takeR :: Int -> [a] -> [a]
        takeR n = reverse . take n . reverse 

        mapListsR ::[Rank] -> [[Rank]]
        mapListsR l = map (\a -> takeR a l ) [1..length l]

        mapListsL :: [Rank] -> [[Rank]]
        mapListsL l = map (\a ->take a l) [1.. length l]

        listOfRanked:: Suit-> [Card] -> [[Rank]]
        listOfRanked s deck = rmdups ((mapListsR $ ranked s deck ) 
        ++ (mapListsL $ ranked s deck))
        ```

    5. I created a helper function to help convert the Cards in my hand to Straight Melds

        ```haskell
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
        ```

    6. Helper function to check if group of ranks are consecutive

        ```haskell
        -- Returns a boolean value True if the consecutive sequence has a length of three or More
        -- Else return False
        checkThreeConsecutiveOrMore:: [Rank]->Bool
        checkThreeConsecutiveOrMore l = ((fromEnum . last) l - (fromEnum . head) l) == (length l - 1) && length l >2
        ```

    7. Now that I have the conditional check to see if the group of cards are consecutive, I can now make the master function that will return a list of lists of cards that form straights. 

        ```haskell
        -- Function that outpus groups of consecutive Cards that are length 3 or more 
        checkRankedConsecutive:: Suit -> [Card] -> [[Card]]
        checkRankedConsecutive s deck =  filter(not.null) $
         map (\combo -> if checkThreeConsecutiveOrMore combo then convertSetToCard s combo else [] ) 
        $ listOfRanked s deck
        ```

    8. I had trouble with being able to form separate Straights in the same suit due to the fact that there is the problem of not having one card to be fit into 2 different straights, therefore I have changed my initial strategy of forming separate straights to forming the longest straight possible. Due to my strategy of forming the longest Straight if possible, I have used maximumBy to get the longest group of cards that form straights. I then use the convertStraightMelds function to directly convert them to Straight Melds. 

        ```haskell
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

        -- MASTER FUNCTION for getting all Straight3, Straight4 and Straight 5 from Player's Hand
        -- Concatenates output from getStraightfromSuit from a certain suit together to form master list of straights
        -- from each suit
        getStraightForAllSuit:: [Card]->[Meld]
        getStraightForAllSuit deck = concat (flip getStraightfromSuit deck <$> [Spade .. ])
        ```

    9. Finally, I am able to combine all these functions together including one function to directly convert cards to deadwood together. I have used multiple guards to check for different conditions while making my Melds. I have used the "\\" operator in order to discard the cards that have already formed sets or straights so that it would be easier for me to convert the remaining cards into Deadwood. 

        Procedure: 

        1. Check if there are more Set3 Set4 Melds compared to Straight Melds and form the Set Melds first- then only form straights with the remaiing cards, lastly form Deadwood with the remaining cards.

        2. Check if there are more Straights than Sets and form Straight Melds and with the remaining cards - form Straight Melds

        3. Check if there are equal amount of Sets and Straights, if yes then form Sets first before Straights

        4. If there are 0 sets and 0 straights, directly form all deadwood

        ```haskell
        makeMelds :: MeldFunc
        makeMelds _ _ deck 
            | length (formAllSetMeld deck) > length(getStraightForAllSuit deck)  = 
        formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
            | length (getStraightForAllSuit deck)> length (formAllSetMeld deck) = 
        getStraightForAllSuit deck ++ formAllSetMeld discardStraightNoSet ++ convertHandtoDeadwood finaldeadwood
            | length(formAllSetMeld deck)== 
        length (getStraightForAllSuit deck )= formAllSetMeld deck ++ getStraightForAllSuit remainder ++ convertHandtoDeadwood discardStraight
            | length (getStraightForAllSuit deck)==0  && length(formAllSetMeld deck)== 0 = 
        convertHandtoDeadwood deck
            | otherwise = convertHandtoDeadwood deck
            where 
                remainder = deck \\ remainderCards deck
                discardStraight = remainder \\ getDiscardofStraight remainder
                discardStraightNoSet = deck\\getDiscardofStraight deck
                finaldeadwood= discardStraightNoSet \\ remainderCards discardStraightNoSet
        ```

2. **Building the playCard function**
    1. For the playCard function, my strategy is to always discard the maximum deadwood card I have in my hand. I have also used memory to check if the score has changed from the previous rounds as well as to check what was the previous action of the player. 

        Procedure:

        1. Check if after dropping highest card that all melds are formed ,can call Gin

        2. Check if after forming melds with all 11 cards, there is only 1 deadwood card left (this card cannot be same as picked card) means can call Gin

        3. check if among the deadwood, there are total points of deadwood <10 , can Knock

        4. Drop Highest deadwood card

        I mainly use my makeMelds function to check if all sets have been formed, or if that after using the makeMelds function there are <=10 points in deadwood which will help me decide if I should call Gin or Knock. I have used the "\\" operator throughout to help me subtract from lists. I have also created small functions such as maxByRank to help me check for the highest ranking Deadwood to discard. 

        The maxByRank function uses Foldable in order to quickly traverse the list of Cards to obtain the Highest Ranking card.

        ```haskell
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

        -- Getting max card
        -- Will be used later to get the highest rank deadwoodcard in your hand
        maxByRankMeld :: [Meld] -> Meld
        maxByRankMeld= foldr1 (\meld_a meld_b -> if getRankMeld meld_a>= getRankMeld meld_b then meld_a else meld_b)
        ```

3. **Building the pickCard function**

    This was the easiest function to build after the playCard and makeMeld function as I only need to have guards to check for **Meldable topCard**. 

    ```haskell
    pickCard :: ActionFunc
    pickCard topDiscard score memory _ hand  
        | or(checkTopCardThreeSet topDiscard hand)==True= (Discard, convertedmemory)
        | or (checkTopCardTwoSet topDiscard hand)==True= (Discard, convertedmemory)
        | or (checkTopCardConsecutiveTwo topDiscard hand)==True = (Discard,convertedmemory)
        | or (checkTopCardConsecutiveThreeFour topDiscard hand)==True= (Discard,convertedmemory)
        | otherwise= (Stock,convertedmemory)
        where convertedmemory= justToStr memory ++ show score
    ```

# Memory 🧠

I needed to implement my memory in such a way to be able to store the **Previous Round Score** and the **List of Previous Actions** taken by the player.

In order to this, I implemented an ADT for the Scores. The Parser combinators are input and output as Strings. 

BNF for Numbers

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<number> ::= <digit> | <number> <digit>

**Due to the Scores having the representation of EG: (30,50) , I have derived a Parser Combinator for scores like so:**

Making the BNF grammar for Single Digits and Double Digits

thisMany :: Int -> Parser a -> Parser [a]
thisMany n p = sequence (replicate n p)

singleDigit :: Parser [Char]
singleDigit = thisMany 1 digit

doubleDigit :: Parser [Char]
doubleDigit= thisMany 2 digit

Do Notation for Single Score EG: (5,5)

singleScore ::Parser[Char]
singleScore = do
    spaces
    _ <- is '('
    a <-singleDigit
    _ <- is ','
    b <- singleDigit
    _ <- is ')'
    pure ("0"++a++"0"++b)

Do notation for Double Score EG: (20,50)

doublescores :: Parser [Char]
doublescores = do
  spaces
  _ <- is '('
  a <- doubleDigit
  _ <- is ','
  b <- doubleDigit
  _ <- is ')'
  pure (a++b)

Do notation for SingleDouble Scores and Double Single Scores in the form of (5,15) or (15,5)

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

Finally , I can combine all the Score Parsers to form the Master Parser Combinator for scores.

-- Master parser combinator to parse 
-- EG: (0,0), (0,40), (30,0) (30,30)
allscores:: Parser[Char]
allscores = singleScore|||singleDoubleScore|||doubleSingleScore|||doublescores

Regarding the list of actions taken, I have made ADTs for Knock, Gin and Drop like so: 

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

Finally the final form of round having being played would be in the form of:

"(0,0)drop"

-- Parser Combinator for All Memory should consist of scores and action taken 
allMemory :: Parser[[Char]]
allMemory = do
    spaces
    a<-allscores
    b<-playerAction
    pure ([a,b])

Therefore we can check if it is in the first turn of any round, as the previous round score would always be behind the new score of the current round

"<previousMemory><score>". 

From here we can check to see if the previousScore is different from the current round's score and therefore decide whether to always drop on first round. 

Scenario: It is already the first turn of the new round.

memory = "(0,0)drop(0,0)drop(0,30)"

When calling **parse (list allMemory) memory**

It will return :

**Result >(0,30)< [["0000","drop"],["0000","drop"]]**

We will be obtain the most recent previous memory by calling Head of the allMemory list. As a result, we can compare the scores and check if it has changed.
