import Data.String
import Data.List

-- Node represents either a non-terminal or terminal symbol, or the special
-- epsilon type which represents the empty derivation.
data Node = N String | T String | Epsilon

-- The lhs of a rule must be a non-terminal.
data Rule = Rule { lhs :: Node
                 , rhs :: [[Node]]
                 }

-- Make this a type so that we can make it part of the Show typeclass.
data Grammar = Grammar [Rule]

instance Show Node where
    show (N n) = "<" ++ n ++ ">"
    show (T t) = t
    show Epsilon = "epsilon"

instance Eq Node where
    (==) (N n1) (N n2) = n1 == n2
    (==) (T n1) (T n2) = n1 == n2
    (==) Epsilon Epsilon = True
    (==) _ _ = False

instance Show Rule where
    show Rule{lhs = nt, rhs = l} =
        show nt ++ " -> " ++ (intercalate " | " $ map show rules)
            where rules = map concat $ map (map (\r -> show r)) l

instance Show Grammar where
    show (Grammar []) = ""
    show (Grammar [r]) = show r
    show (Grammar (rule:rl)) = show rule ++ "\n" ++ show (Grammar rl)

-- Helper function for firstSet which uses continuation passing to deal with
-- epsilons.
firstSetProd :: Grammar -> [Node] -> Node -> [Node]
firstSetProd (Grammar rules) _ (T s) = [T s]
firstSetProd (Grammar rules) (h:tl) Epsilon =
    firstSetProd (Grammar rules) tl h
firstSetProd (Grammar rules) _ Epsilon = []
firstSetProd (Grammar rules) cont (N symbol) =
    let production = find (\r -> lhs r == (N symbol)) rules
     in case production of 
          Just (Rule {rhs=r}) -> concat $
              map getFirst r where
                  getFirst (s:ss) =
                      firstSetProd (Grammar rules) (ss ++ cont) s

-- Given a grammar and a symbol, return a list of the first set of the symbol
-- according to the given grammar.
firstSet :: Grammar -> Node -> [Node]
firstSet g n = nub $ firstSetProd g [] n

-- Filters down a grammer to only the rules which contain the given non-terminal
-- on the rhs.
relevantRules :: Grammar -> Node -> Grammar
relevantRules (Grammar g) (N nt) =
    Grammar $ filter (\rule -> concat (rhs rule) /= []) $
        map (\rule -> Rule { lhs = lhs rule
                                 , rhs = filter (elem (N nt)) (rhs rule)
                                 }) g

followSetProd :: Grammar -> Node -> [Node] -> Node -> [Node]
followSetProd g lhs prod node =
    let indices = elemIndices node prod
     in concat $ map (\i -> if i < (length prod) - 1
                               then firstSetProd g (drop (i+2) prod) $
                                   prod !! (i + 1)
                      else followSet g lhs
                     )
            indices

followSetRule :: Grammar -> Rule -> Node -> [Node]
followSetRule g rule node =
    nub . concat $ map (\r -> followSetProd g (lhs rule) r node) (rhs rule)

-- First create an augmented grammar which has imposes that "$" is in the follow
-- set of the start symbol.
followSet :: Grammar -> Node -> [Node]
followSet (Grammar g) (N nt) =
    let augGrammar = Grammar (Rule { lhs = (N "_Start")
                                   , rhs = [[lhs (head g), T "$"]]
                                   } : g)
        relRules = relevantRules augGrammar (N nt)
     in case relRules of
          Grammar [] -> []
          Grammar [r] -> followSetRule augGrammar r (N nt)
          Grammar (r:rs) -> nub $ (followSetRule augGrammar r (N nt)) ++
              (followSet (Grammar rs) (N nt))
