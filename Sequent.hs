module Sequent ( Name,
               Prop (Var, Not, (:||:), (:&&:), (:->:), (:<->:)),
               Sequent ( (:|=:) ) )

where

import Control.Monad
import Test.QuickCheck

type Name = String

data Prop = Var Name
          | Not Prop
          | Prop :||: Prop
          | Prop :&&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq,Ord)

data Sequent = [Prop] :|=: [Prop]
          deriving (Eq,Ord)

infix 0 :|=:

showProp :: Prop -> String
showProp (Var x)     = x
showProp (Not p)     = par ("not " ++ showProp p)
showProp (p :||: q)  = par (showProp p ++ " || " ++ showProp q)
showProp (p :&&: q)  = par (showProp p ++ " && " ++ showProp q)
showProp (p :->: q)  = par (showProp p ++ " -> " ++ showProp q)
showProp (p :<->: q) = par (showProp p ++ " <-> " ++ showProp q)

par :: String -> String
par s  =  "(" ++ s ++ ")"

showSequent (as :|=: ss) = (show as) ++ " |= " ++ (show ss)

instance Show Prop
  where show = showProp

instance Show Sequent
  where show = showSequent

-- for quickCheck testing on Prop

instance Arbitrary Prop where
  arbitrary = sized prop
      where
        prop n | n <= 0    = oneof [ return (Var "a"),
                                     return (Var "b"),
                                     return (Var "c"),
                                     return (Var "d"),
                                     return (Var "e"),
                                     return (Var "f")
                                   ]
               | otherwise = oneof [ liftM Not p2
                                   , liftM2 (:||:) p2 p2
                                   , liftM2 (:&&:) p2 p2
                                   , liftM2 (:->:) p2 p2
                                   , liftM2 (:<->:) p2 p2
                                  ]
               where
                 p2  =  prop (n `div` 4)

-- for quickCheck testing on Sequent

instance Arbitrary Sequent where
  arbitrary = liftM2 (:|=:) arbitrary arbitrary

