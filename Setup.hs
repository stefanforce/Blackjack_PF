module Setup where


import System.Random

--Definitions of a card

data Card = Card Rank Suit
            deriving (Eq, Show)

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s


data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)


data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

--Definitions of a hand (a deck is also a big hand)

data Hand = Empty | Add Card Hand deriving(Show,Eq)


                      
empty :: Hand
empty = Empty

--Giving value to cards

valueRank :: Rank -> Int
valueRank (Numeric i) = i
valueRank Ace         = 11
valueRank _           = 10     


valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

--Counting the value of a hand

valuehand :: Hand -> Int
valuehand Empty            = 0
valuehand (Add card hand) = (valueCard card) + (valuehand hand)     

--Also takes care what happens when you have aces in hand

value :: Hand -> Int
value hand | valuehand hand > 21 = valuehand hand - (10 * numberOfAces hand)
           | otherwise = valuehand hand

--Counts the number of aces in your hand

numberOfAces :: Hand -> Int
numberOfAces Empty = 0
numberOfAces (Add (Card Ace s) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

--Defines when the game is instantly over for a player

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--Definitions of the players/bank and when they are the winners

data Player = Player1 | Player2 | Bank
              deriving (Show, Eq)

winner :: Hand -> Hand -> Hand -> Player
winner handP1 handP2 handB | gameOver(handP1) && gameOver(handP2)= Bank
                           | gameOver(handB) && gameOver(handP2) = Player1
                           | gameOver(handB) && gameOver(handP1) = Player2
                           | (value(handP1) > value(handB)) && (value(handP1) > value(handP2))= Player1
                           | (value(handP2) > value(handB)) && (value(handP2) > value(handP1))= Player2
                           | (value(handB) >= value(handP1)) && (value(handB) >= value(handP2))= Bank
                           
--combines 2 stacks of cards. (i.e.: a hand of (1,2) and a hand of (3,4) combines into a big hand of (1,2,3,4) )

addStack :: Hand -> Hand -> Hand
addStack Empty hand2 = hand2
addStack (Add card hand1)  hand2 = (Add card (addStack hand1 hand2) )

--Adds all cards of a certain suit into 1 big stack

handSuit :: Suit -> Hand
handSuit s = (Add (Card Ace s) (Add (Card King s) (Add (Card Queen s) (Add (Card Jack s) (Add (Card (Numeric 10) s) (Add (Card (Numeric 9) s) (Add (Card (Numeric 8) s) (Add (Card (Numeric 7) s) (Add (Card (Numeric 6) s) (Add (Card (Numeric 5) s) (Add (Card (Numeric 4) s) (Add (Card (Numeric 3) s) (Add (Card (Numeric 2) s)  Empty)))))))))))))

--Definition of a full deck

fullDeck :: Hand
fullDeck = (handSuit Spades) `addStack` (handSuit Hearts) `addStack` (handSuit Clubs) `addStack` (handSuit Diamonds)

--Function which draws a card for a player

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error ("draw: The deck is empty.") 
draw (Add card hand1) hand2 = (hand1 , (Add card hand2))

--Function which draws a card for the bank. It must always draw a card when it's hand's value is below 16

bankDraw' :: Hand -> Hand -> Hand
bankDraw' deck bankHand | value bankHand < 16 = bankDraw' deck' bankHand'
                        | otherwise = bankHand
  where (deck' , bankHand') = draw deck bankHand  

bankDraw :: Hand -> Hand
bankDraw deck = bankDraw' deck Empty

--Helper functions which remove and pick a card from the deck (used for shuffling)

removeCard :: Hand -> Int -> Hand
removeCard (Add card hand) n | n == 1 = hand
                             | otherwise = (Add card (removeCard hand (n-1)))


pickCard :: Hand -> Int -> Card
pickCard (Add card hand) n | n == 1 = card
                            | otherwise = pickCard hand (n-1)

--Function that shuffles a deck( Picks a random card from the deck and puts it on top)

shuffle :: StdGen -> Hand -> (Hand,StdGen)
shuffle g deck = (Add (pickCard deck n') (removeCard deck n'),g')
  where (n',g') = randomR (1,52) g 

first :: (Hand,StdGen) -> Hand
first (a,b) = a

second :: (Hand,StdGen) -> StdGen
second (a,b) = b
  
--Function that does the shuffling process a number set of times

massShuffle :: Hand -> Int -> StdGen -> Hand
massShuffle hand 1 g = first (shuffle g hand)
massShuffle hand n g = case (shuffle g hand) of
                        (a,b) -> massShuffle a (n-1) b


--Function that displays the cards and hand

display :: Hand -> String
display Empty = ""
display (Add card hand) = case display hand of
                            "" ->  (displayCard card) ++ (display hand)
                            _ -> (displayCard card) ++ "," ++ (display hand)


displayCard :: Card -> String 
displayCard card = case isNumeric (rank card) of
                      True -> "(" ++ (show (removeNumeric (rank card))) ++ " of " ++ (show (suit card)) ++ ")"
                      False -> "(" ++ (show (rank card)) ++ " of " ++ (show (suit card))  ++ ")"
removeNumeric :: Rank -> Int
removeNumeric (Numeric int) = int
 

isNumeric :: Rank -> Bool    
isNumeric Jack = False
isNumeric King = False
isNumeric Queen = False
isNumeric Ace = False
isNumeric _ =True

--Test hands

testHand21 :: Hand
testHand21 =  (Add (Card (Numeric 2) Spades) (Add (Card (Numeric 1) Diamonds) Empty))

testHandBig = (Add (Card Ace Spades) (Add (Card Jack Diamonds) (Add (Card King Diamonds) Empty)))
    