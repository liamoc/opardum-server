module Main where

import Test.QuickCheck
import Opardum.OperationalTransforms
import Control.Applicative((<$>))
import Data.Maybe
import Debug.Trace

instance Arbitrary OpComponent where
   arbitrary = oneof [randomInsert, randomDelete, randomRetain]
      where  randomInsert = Insert <$> arbitrary 
             randomRetain = Retain <$> (arbitrary  `suchThat` (> 0))
             randomDelete = Delete <$> arbitrary
   shrink (Insert v) = map Insert (shrink v)
   shrink (Retain v) = map Retain (shrink v)
   shrink (Delete v) = map Delete (shrink v)

composition_assoc :: Op -> Op -> String -> Property 
composition_assoc op1 op2 doc = ([Insert doc] +> op1) /= Nothing 
                             && (op1 +> op2) /= Nothing ==> (fromJust ([Insert doc] +> op1) +> op2) == ([Insert doc] +> fromJust (op1 +> op2))

composition_assoc2 :: Op -> Op -> String -> Property
composition_assoc2 op1 op2 doc = (op1 +> op2) == Nothing ==> let foo = [Insert doc] +> op1 
                                                             in foo == Nothing || (fromJust foo +> op2) == Nothing

main = mapM_ quickCheck [composition_assoc, composition_assoc2]


