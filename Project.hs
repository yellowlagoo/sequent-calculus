module Project ( prove, proveCount ) where

import Sequent

import Data.List

prove :: Sequent -> [Sequent]
prove (left :|=:  right)| any isOr right = concatMap prove (orR (left :|=: right))
                        | any isAnd right = concatMap prove (andR (left :|=: right))
                        | any isNot right = concatMap prove (notR (left :|=: right))
                        | any isImp right = concatMap prove (impR (left :|=: right))
                        | any isBi right = concatMap prove (biR (left :|=: right))
                        | any isOr left = concatMap prove (orL (left :|=: right))
                        | any isAnd left = concatMap prove (andL (left :|=: right))
                        | any isNot left = concatMap prove (notL (left :|=: right))
                        | any isImp left = concatMap prove (impL (left :|=: right))
                        | any isBi left = concatMap prove (biL (left :|=: right))
                        | otherwise = nub (identity (left :|=: right))
                    where
                        isOr :: Prop -> Bool 
                        isOr (p :||: q) = True
                        isOr _ = False

                        isAnd :: Prop -> Bool
                        isAnd (p :&&: q) = True
                        isAnd _ = False 

                        isNot :: Prop -> Bool
                        isNot (Not p) = True
                        isNot _ = False

                        isImp :: Prop -> Bool
                        isImp (p :->: q) = True
                        isImp _ = False

                        isBi :: Prop -> Bool
                        isBi (p :<->: q) = True
                        isBi _ = False

-- for challenge part
proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined

type Rule = Sequent ->  [Sequent]
orL, orR, andL, andR, notL, notR, identity, impL, impR, biL, biR :: Rule
orR  = applyR orR 
orL  = applyL orL 
andR  = applyR andR  
andL  = applyL andL  
notR  = applyR notR  
notL  = applyL notL  
impL  = applyL impL  
impR  = applyR impR  
biL  = applyL biL  
biR  = applyR biR  
identity (left :|=: right) | any (`elem` right) left  =  []
                           | otherwise =  [sort (nub left) :|=: sort (nub right)]

applyR :: (Sequent -> [Sequent]) -> Sequent -> [Sequent]
applyR orR ((left :|=: ((p :||: q) : ps)) ) = [left :|=: (p : q : ps)]
applyR andR (left :|=: (p1 :&&: p2) : ps) =  [left :|=:  (p1 : ps), left :|=: (p2 : ps)]
applyR notR (left :|=: (Not p1 : ps)) =  [left ++ [p1] :|=: ps]
applyR impR (left :|=: ((p1 :->: p2) : ps)) = [(left ++ [p1]) :|=: (p2 : ps)]
applyR biR (left :|=: (p1 :<->: p2) : ps) = (left :|=: (p1 :->: p2) : ps) : [left :|=: (p2 :->: p1) : ps]
applyR rule (left :|=: (p : ps)) = [left :|=: (p : newPs) | (left :|=: newPs) <- rule (left :|=: ps)]

applyL :: (Sequent -> [Sequent]) -> Sequent -> [Sequent]
applyL orL ((p0 :||: p1) : ps :|=: right) =  [(p0: ps) :|=: right, (p1: ps) :|=: right]
applyL andL ((p0 :&&: p1) : ps :|=: right) =  [(p0 : p1 : ps) :|=: right]
applyL notL ((Not p0 : ps) :|=: right) =  [ps :|=: right ++ [p0]]
applyL impL ((p0 :->: p1) : ps :|=: right) = (ps :|=: (p0 : right)) : [(p1 : ps) :|=: right]
applyL biL ((p0 :<->: p1) : ps :|=: right) = [ps ++ [p0 :->: p1] ++ [p1 :->: p0] :|=: right]
applyL rule ((p : ps) :|=: right) =  [(p : newPs) :|=: right | (newPs :|=: right) <- rule (ps :|=: right)]
