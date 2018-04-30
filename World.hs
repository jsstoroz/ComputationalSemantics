{-# LANGUAGE FlexibleInstances #-}

module World where

import HRAS
import P
import Lexicon

type Prop = String

data ModalOperator = Necessarily | Possibly deriving (Show, Eq)

data CTLOperator = EX | EW | EF | EG | AX | AW | AF | AG deriving (Show, Eq)

data LogicalOperator = NOT | AND | OR | ARROW | BICONDITIONAL deriving (Show, Eq)

 
--prop needs to be [prop] for W operator
data CTLProp = CTLProp { ctlOp :: CTLOperator,
                         prop ::  [Prop] } deriving (Show, Eq)

data CTLLogProp = CTLLogProp { ctlOp1 :: CTLOperator, 
                               prop1 :: Prop, 
                               logOp :: LogicalOperator, 
                               prop2 :: Prop } deriving (Show, Eq)
                 

data World = World { name :: String, -- Adding name just makes it easier to test
                     propositions :: [Prop] } deriving (Show, Eq)

-- Worlds and test set

w1 = World { name = "w1", propositions = ["i nudge alice", "we receive some swords", "the wizard comes to the princess", "the boys play with the daggers", "few giants sit", "the dwarves believe snowwhite", "several giants shout", "they return from the castle", "that person alarms goldilocks", "those boys admire atreyu", "some girls admire princesses", "several girls see the giant"] }
w2 = World { name = "w2", propositions = ["the boys play with the daggers", "the boys break the daggers", "every woman draws a sword", "the dwarves believe snowwhite", "those boys admire atreyu", "some girls admire princesses", "several girls see the giant", "the men choose daggers", "you stay with goldilocks", "we receive some swords"] }
w3 = World { name = "w3", propositions = ["the dwarves believe snowwhite", "several girls see the giant", "atreyu writes to the man", "we speak to dorothy", "alice reasons with the wizard", "this dwarf lives", "few giants sit", "i nudge alice", "snowwhite annoys the dwarves", "that person alarms goldilocks", "some girls admire princesses"] }
w4 = World { name = "w4", propositions = ["we speak to dorothy", "alice reasons with the wizard", "many wizards work with atreyu", "many princesses know atreyu", "you stay with goldilocks", "some girls admire princesses", "a wizard finds a dagger", "the dwarves believe snowwhite", "they return from the castle", "every woman draws a sword"] }
w5 = World { name = "w5", propositions = ["we speak to dorothy", "they return from the castle", "some girls admire princesses", "many wizards work with atreyu", "a wizard finds a dagger", "this dwarf lives", "you stay with goldilocks", "several giants shout", "the dwarves believe snowwhite", "many princesses know atreyu"] }
w6 = World { name = "w6", propositions = ["the dwarves believe snowwhite", "the boys find alice", "that person alarms goldilocks", "those boys admire atreyu"] }
w7 = World { name = "w7", propositions = ["the dwarves believe snowwhite", "this dwarf lives", "i nudge alice", "atreyu writes to the man", "the boys play with the daggers", "they return from the castle"] }
w8 = World { name = "w8", propositions = ["the dwarves believe snowwhite", "some girls admire princesses", "we speak to dorothy", "i nudge alice", "atreyu writes to the man"] }
w9 = World { name = "w9", propositions = ["the dwarves believe snowwhite", "several giants shout", "this dwarf lives", "many princesses know atreyu", "few giants sit"] }
w10 = World { name = "w10", propositions = ["the dwarves believe snowwhite", "the boys find alice", "several giants shout", "that person alarms goldilocks", "many princesses know atreyu", "few giants sit"] }
w11 = World { name = "w11", propositions = ["the dwarves believe snowwhite", "some girls admire princesses", "we speak to dorothy", "the boys find alice"] }
w12 = World { name = "w12", propositions = ["the dwarves believe snowwhite", "some girls admire princesses", "we speak to dorothy", "the boys find alice", "that person alarms goldilocks"] }

test1 = CTLProp AG ["the dwarves believe snowwhite"]  -- isSatified test1 w1 = True, isValid test1 = True, isSatisfiable test1 = True
test2 = CTLProp AG ["some girls admire princesses"]  -- isSatified test2 w1 = False, isValid test2 = False
test3 = CTLProp EG ["some girls admire princesses"]  -- isSatified test3 w1 = True, isValid test3 = False, isSatisfiable test3 = True
test4 = CTLProp EG ["we speak to dorothy"]  -- isSatified test4 w5 = True, isSatisfied test4 w6 = False
test5 = CTLProp AX ["this dwarf lives"]  -- isSatified test5 w4 = True, isSatified test5 w5 = False
test6 = CTLProp AX ["that person alarms goldilocks"]  -- isSatified test6 w6 = False
test7 = CTLProp EX ["this dwarf lives"]  -- isSatisfied test7 w3 = True, isSatisfied test7 w7 = False
test8 = CTLProp EX ["every woman draws a sword"]  -- isSatisfied test8 w5 = False
test9 = CTLProp AF ["many princesses know atreyu"]  -- isSatisfied test9 w2 = True, isSatisfied test9 w11 = False
test10 = CTLProp EF ["this dwarf lives"]  -- isSatified test10 w3 = True, isSatified test10 w11 = False
test11 = CTLProp AW ["several girls see the giant", "snowwhite annoys the dwarves"]  -- isSatisfied test11 w1 = True ! 
test12 = CTLProp AW ["we receive some swords", "a wizard finds a dagger"]  -- isSatisfied test12 w1 = False !
test13 = CTLProp EW ["several girls see the giant", "those boys admire atreyu"]  -- isSatisfied test13 w1 = True
test14 = CTLProp EW ["atreyu writes to the man", "the men choose daggers"]  -- isSatisfied test14 w3 = False

test15 = CTLLogProp AG "the dwarves believe snowwhite" AND "some girls admire princesses"  -- isSatisfied test15 w5 = True, isSatisfied test15 w1 = False
test16 = CTLLogProp EG "the dwarves believe snowwhite" OR "some girls admire princesses"  -- isSatisfied test16 w1 = True, isValid test16 = True
test17 = CTLLogProp EG "the dwarves believe snowwhite" AND "some girls admire princesses"  -- isSatisfiable test17 = True, isValid test17 = False
test18 = CTLLogProp EX "few giants sit" ARROW "several girls see the giant"  -- isSatisfied test18 w2 = True, isSatisfied test18 w6 = False
test19 = CTLLogProp AX "every woman draws a sword" BICONDITIONAL "the boys play with the daggers"  -- isSatisfied test19 w1 = True, isSatisfied test19 w4 = False
test20 = CTLLogProp AF "they return from the castle" NOT ""  -- isSatisfied test20 w2 = True, isSatisfied test20 w4 = False

model = [(w1, w2), (w2, w3), (w3, w4), (w3, w5), (w3, w6), (w4, w7), (w5, w8), (w6, w9), (w6, w10), (w8, w11), (w11, w12)]
all_worlds = [w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12]

class IsValid a where
    isValid :: a -> Bool
instance IsValid CTLProp where
-- isValid x = foldl (\acc w -> (isSatisfied x w) && acc) True model 
    isValid x = and [isSatisfied x w | w <- all_worlds]
instance IsValid CTLLogProp where
    isValid x = and [isSatisfied x w | w <- all_worlds]

class IsSatisfiable a where
    isSatisfiable :: a -> Bool
instance IsSatisfiable CTLProp where
    isSatisfiable x = or [isSatisfied x w | w <- all_worlds]
instance IsSatisfiable CTLLogProp where 
    isSatisfiable x = or [isSatisfied x w | w <- all_worlds]

{-
Operators over all paths
E : E phi = there EXISTS at least one path starting from current world along which
	phi is true at some point
A : A phi = phi true at some point along ALL paths starting from current world

Path-specific operators
X : X phi = phi true at the NEXT state
W : phi W psi = phi holds WEAKLY (UNTIL) psi becomes true. sometimes read "unless"
F : F phi = phi FINALLY true somewhere along subsequent path
G : G phi = phi true GLOBALLY at all points along subsequent path

EX : phi holds in at least one of the next worlds from current world
EW: : there exists at least one path starting from the current world where phi holds until psi is true
EF : phi is eventually true somewhere along any path starting from the current world
EG : there exists one path starting from current world where phi is true in all states along path
AX : phi holds in all the next worlds from current world
AW: along all paths starting from the current world
-}

class IsSatisfied a where
    isSatisfied :: a -> World -> Bool

instance IsSatisfied CTLProp where
    isSatisfied t w
    --									EX : phi holds in at least one of the next worlds from current world
                -- or ::= foldr (||) False xn
         | (ctlOp t) == EX = or [p `inWorld` world | world <- nextWorlds w]
    -- 									EW: : there exists at least one path starting from the current world where phi holds until psi is true
                        --two propostions
         | (ctlOp t) == EW = let q = prop t !! 1
                             in untilOr p q w
    --									EF : phi is eventually true somewhere along any path starting from the current world
         | (ctlOp t) == EF = or [or [p `inWorld` world | world <- path] | path <- paths]
         --							EG : there exists one path starting from current world where phi is true in all states along subsequent path
         | (ctlOp t) == EG = or [and [p `inWorld` world | world <- path] | path <- paths]
         --							AX : phi holds in all the next worlds from current world
         --and ::= foldr (&&) True xn
         | (ctlOp t) == AX = and [p `inWorld` world | world <- nextWorlds w]
         --							AW: along all paths starting from the current world, phi holds true until psi is true
         -- two propositions
         | (ctlOp t) == AW = let q = prop t !! 1
                             in untilAnd p q w
         --							AF : phi is eventually true (true at some point) along all paths starting from current world
         | (ctlOp t) == AF = and [or [p `inWorld` world | world <- path] | path <- paths]
         --							AG : along all paths starting from the current world, phi is true at all points along the subsequent path
         | (ctlOp t) == AG = and [and [p `inWorld` world | world <- path] | path <- paths]
         where p = prop t !! 0
               paths = [path | path <- findAllPaths w]
    
    

--function to get list of next worlds
--snd if fst = w in model
nextWorlds :: World -> [World]
nextWorlds w = [snd tup | tup <- model, fst tup == w]

--recursive function

-- if p true inWorld w+1 and q not in nextWorlds w+1 ...

untilOr :: Prop -> Prop -> World -> Bool
untilOr p q w
--if p not inWorld w, evaluate false (base case)
    | (inWorld p w) == False = False
    --if p true inWorld w and q in nextWorlds w, evaluate true
    |  pOrq = True
    -- if p true inWorld w and q not in nextWorlds w, recurse
    |  pOrq == False = or [untilOr p q w1 | w1 <- nw]
    | otherwise = False
    where nw = nextWorlds w
          pOrq = or [q `inWorld` world | world <- nw]
    
untilAnd :: Prop -> Prop -> World -> Bool
untilAnd p q w
    --if p not inWorld w, evaluate false (base case)
    | (inWorld p w) == False = False
    --if p true inWorld w and q in ALL nextWorlds w, evaluate true
    | pAndq = True
    -- if p true inWorld w and q not in nextWorlds w, recurse
    | pOrq == False = or [untilAnd p q w1 | w1 <- nw]
    | otherwise = False
    where nw = nextWorlds w
          pAndq = and [q `inWorld` world | world <- nw]
          pOrq = or [q `inWorld` world | world <- nw]

--helper function to find all the paths from current world
findAllPaths :: World -> [[World]]
findAllPaths w = [tail worldList | worldList <- allPaths]
    where allPaths = findAllPathsAux w

-- Recursive function for findAllPaths
findAllPathsAux :: World -> [[World]]
findAllPathsAux w 
    | length continuations == 0 = [[w]]
    | length continuations == 1 = [[w] ++ path | path <- (head continuations)]
    | otherwise = flatten [[[w] ++ path | path <- cont] | cont <- continuations]
    where nw = nextWorlds w
          continuations = [(findAllPathsAux w1) | w1 <- nw]
--if w is the first element in a tuple, add the second element to the list
--if the first element appears more than once in the model make a new path copied from the original

-- helper function for findAllPaths - flattens nested lists one level
flatten :: [[a]] -> [a]
flatten [] = []
flatten x = foldl (++) [] x

class InWorld a where
    inWorld :: a -> World -> Bool
instance InWorld Prop where
    inWorld p w = p `elem` (propositions w)

--LOGICAL OPERATORS
--NOT, AND, OR, ARROW, BICONDITIONAL

instance IsSatisfied CTLLogProp where
    isSatisfied t w
    -- EX : phi holds in at least one of the next worlds from current world
          -- or ::= foldr (||) False xn
         | (ctlOp1 t) == EX = or [t `inWorld` world | world <- nextWorlds w]
    -- EF : phi is eventually true somewhere along any path starting from the current world
         | (ctlOp1 t) == EF = or [or [t `inWorld` world | world <- path] | path <- paths]
         --							EG : there exists one path starting from current world where phi is true in all states along subsequent path
         | (ctlOp1 t) == EG = or [and [t `inWorld` world | world <- path] | path <- paths]
         | (ctlOp1 t) == AX = and [t `inWorld` world | world <- nextWorlds w]
         --							AF : phi is eventually true (true at some point) along all paths starting from current world
         | (ctlOp1 t) == AF = and [or [t `inWorld` world | world <- path] | path <- paths]
         --							AG : along all paths starting from the current world, phi is true at all points along the subsequent path
         | (ctlOp1 t) == AG = and [and [t `inWorld` world | world <- path] | path <- paths]
         where paths = [path | path <- findAllPaths w]

arrowOp :: Bool -> Bool -> Bool
arrowOp True False = False
arrowOp _ _ = True

biconditionalOp :: Bool -> Bool -> Bool
biconditionalOp p q
    | p == q = True
    | otherwise = False
  
-- Tests if a combination of props is true in a given world
instance InWorld CTLLogProp where
    inWorld p w
        | op == NOT = not p1
        | op == AND = p1 && p2
        | op == OR = p1 || p2
        | op == ARROW = arrowOp p1 p2
        | op == BICONDITIONAL = biconditionalOp p1 p2
        where op = logOp p
              p1 = (prop1 p) `inWorld` w
              p2 = (prop2 p) `inWorld` w

