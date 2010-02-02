{-# LANGUAGE PatternGuards #-}
module Haskellpad.OperationalTransforms where

import Text.JSON

type Position = Int

type Version = Int

data MyOp = Insert String
          | Delete String
          | Retain Int
          deriving (Show, Eq)

type Packet = (Int, Op)


instance JSON MyOp where
   readJSON (JSObject v) = let assocs  = fromJSObject v 
                            in case lookup "data" assocs of
                                  Nothing -> Error "No data"
                                  Just (JSString strdata) -> case lookup "type" assocs of
                                                     Nothing -> Error "No Type"
                                                     Just (JSString v) -> case fromJSString v of
                                                           "Insert" -> Ok $ Insert $ fromJSString strdata
                                                           "Delete" -> Ok $ Delete $ fromJSString strdata
                                                           "Retain" -> case (reads $ fromJSString strdata) of
                                                             [(v,"")] -> Ok $ Retain v
                                                             _        -> Error "No numeric data!"
                                                           _        -> Error "No standard type!"
                                                     _ -> Error "Invalid type!"

   showJSON = JSObject . toJSObject . zip ["type","data"] . map JSString . map toJSString . words . show' 
        where show' (Insert str) = "Insert " ++ str
              show' (Delete str) = "Delete " ++ str
              show' v = show v

type Op = [MyOp] 

type Doc = [Op]

delete (Delete _) = True
delete _          = False
insert (Insert _) = True
insert _          = False
retain (Retain _) = True
retain _          = False

oplength (Insert v) = length v
oplength (Delete v) = length v
oplength (Retain n) = n

sub n (Delete str) = Delete $ drop n str
sub n (Retain n')  = Retain $ n' - n
sub n (Insert str) = Insert $ drop n str

(<+) :: Op -> Op -> Op
op1s <+ []  = op1s
[] <+ op2s  = op2s
(op1:op1s) <+ op2s       | insert op1 = op1:(op1s <+ op2s)
op1s <+ (op2:op2s)       | delete op2 = op2:(op1s <+ op2s)
(op1:op1s) <+ (op2:op2s) 
     | delete op1, retain op2 = let [l_op1, l_op2] = map oplength [op1, op2]
                                 in case compare l_op1 l_op2 of
                                   EQ -> op1:(op1s <+ op2s)
                                   LT -> op1:(op1s <+ (sub l_op1 op2:op2s))
                                   GT -> (first l_op2 op1):
                                         ((sub l_op2 op1:op1s) <+ op2s)
     | delete op1, insert op2 = let [l_op1, l_op2] = map oplength [op1, op2]
                                 in case compare l_op1 l_op2 of
                                   EQ -> op1s <+ op2s
                                   LT -> op1s <+ (sub l_op1 op2:op2s)
                                   GT -> (sub l_op2 op1:op1s) <+ op2s
     | retain op1, retain op2 = let [l_op1, l_op2] = map oplength [op1, op2]
                                 in case compare l_op1 l_op2 of
                                   EQ -> op1:(op1s <+ op2s)
                                   LT -> op1:(op1s <+ (sub l_op1 op2:op2s))
                                   GT -> (first l_op2 op1):
                                         ((sub l_op2 op1:op1s) <+ op2s) 
     | retain op1, insert op2 = let [l_op1, l_op2] = map oplength [op1, op2]
                                 in case compare l_op1 l_op2 of
                                   EQ -> op2:(op1s <+ op2s)
                                   LT -> (first l_op1 op2):(op1s <+ (sub l_op1 op2:op2s))
                                   GT -> op2:((sub l_op2 op1:op1s) <+ op2s) 
     | otherwise = error "Tried to compose operations that didn't compose :("
     where 
        first n (Insert str) = Insert $ take n str
        first n (Delete str) = Delete $ take n str
        first n (Retain num) = Retain $ n

normalize ((Insert str):(Insert str'):rest) = normalize ((Insert $ str ++ str'):rest)
normalize ((Delete str):(Delete str'):rest) = normalize ((Delete $ str ++ str'):rest)
normalize ((Retain num):(Retain num'):rest) = normalize ((Retain $ num +  num'):rest)
normalize ((Retain 0):rest) = normalize rest
normalize (op1:rest) = op1:(normalize rest)
normalize [] = []

t :: (Op, Op) -> (Op, Op)
t (s, c) = let (a,b) = unzip $ t' s c 
            in (normalize a, normalize b)
  where
    t' (op:ops) (ag:ags) 
       | all retain [op,ag] = let adv = min (oplength op) (oplength ag) 
                               in (Retain adv, Retain adv):subtraction
       | delete op = (op, Retain 0):subtraction
       | delete ag = (Retain 0, ag):subtraction
       | insert op = (op, Retain $ oplength op):(t' ops $ ag:ags)
       | insert ag = (Retain $ oplength ag, op):(t' (op:ops) ags)
       where
         subtraction = let [l_op, l_ag] = map oplength [op, ag]
                        in case compare l_op l_ag of
                             EQ -> t' ops ags 
                             LT -> t' ops $ sub l_op ag:ags
                             GT -> t' (sub l_ag op:ops) ags 
    t' (op:ops) [] = (op, Retain $ oplength op):(t' ops [])                         
    t' [] (ag:ags) = (Retain $ oplength ag, ag):(t' [] ags)
    t' [] [] = []



