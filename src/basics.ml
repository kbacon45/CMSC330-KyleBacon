open Funs 

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let abs x = 
if x < 0 then
-1 * x
else
x
;;

let rev_tup tup = 
match tup with
(a,b,c) -> (c,b,a)
;;

let is_even x = 
if x mod 2 = 0 then
true
else
false
;;

let area point1 point2 =  abs (match point1 with (a,b) -> a -
 match point2 with (a,b) -> a) *
 abs ( (match point1 with (a,b) -> b -
 match point2 with (a,b) -> b) )
 ;;



(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
match n with
0 -> 0
| 1 -> 1
| _ -> (fibonacci (n-1)) + (fibonacci (n-2))

let rec pow x p = 
match p with
0 -> 1
| 1 -> x 
| _ -> x * (pow x (p-1))


let rec logHelp x y z =
if z = y then 1
else if z > y 
then 0
else 1 + (logHelp x y (z*x)  )

let rec log x y = 
(logHelp x y x)


let rec gcfHelp x y z g = 
if z > y || z > x
then g
else
if x mod z = 0 && y mod z = 0
then gcfHelp x y (z+1) z
else gcfHelp x y (z+1) g

let rec gcf x y = 
match x with
0 -> (match y with 0 -> 0 | _ -> y)
| _ -> (match y with 0 -> x | _ -> (gcfHelp x y 1 0))





(*****************)
(* Part 3: Lists *)
(*****************)


let rec reverseHelp lst newLst = 
match lst with
[] -> newLst
| first::rest -> reverseHelp (rest) (first::newLst)


let reverse lst = 
reverseHelp lst []

let combinTups lst1 lst2 =
match lst1 with
(a,b) -> match lst2 with
(c,d) -> (a,b,c,d)


let rec zipHelp lst1 lst2 newLst = 
match lst1 with
[] -> newLst
| first::rest -> match lst2 with
[] -> newLst
| firstTwo::restTwo -> zipHelp rest restTwo (newLst @ 
( (combinTups first firstTwo)::[] )  )

let zip lst1 lst2 = zipHelp lst1 lst2 []



let rec addsort item lst =
match lst with
[] -> item::[]
| first::rest -> 
if first > item
then item::(first::rest)
else 
first::(addsort item rest)


let rec merge lst1 lst2 = 
match lst1 with
[] -> lst2
| first::rest -> merge rest (addsort first lst2)










let rec is_present lst v = 
match lst with
[] -> false
| first::rest ->
if first = v 
then true
else (is_present rest v)


let rec enHelp n lst count desMod = 
match lst with 
[] -> []
| first::rest -> 
if (count mod n) = desMod
then first::(enHelp n rest (count+1) desMod)
else (enHelp n rest (count+1) desMod)


let every_nth n lst = enHelp n lst 1 0

      

let rec fstOLast1 lst = 
match lst with
[] -> []
| curr::rest -> 
 match curr with
(a,b) -> a::(fstOLast1 rest)

let rec fstOLast2 lst = 
match lst with
[] -> []
| curr::rest -> 
 match curr with
(a,b) -> b::(fstOLast2 rest)





let rec jtBraid lst1 lst2 = 
match lst2 with
[] -> []
| curr1::rest1 ->
match lst1 with
[] -> curr1::[]
| curr2::rest2 ->
curr1::(curr2::(jtBraid rest2 rest1))




let jumping_tuples lst1 lst2 = 
let lstA = fstOLast1 lst1 in
let lstB =  fstOLast2 lst2 in
 ( jtBraid (enHelp 2 lstA 0 1) (enHelp 2 lstB 0 0 ) ) @
( jtBraid (enHelp 2 lstB 0 1 ) (enHelp 2 lstA 0 0) ) 
 

let rec countLst lst =
match lst with
[] -> 0
| curr::rest -> 1 + countLst rest



let rec funCall lst output = 
match lst with 
[] -> output
| curr::rest ->
funCall rest (curr output)

let rec rmvElmt lst pos idx = 
match lst with
[] -> []
| curr::rest -> 
if pos = idx 
then rest
else curr::(rmvElmt rest (pos+1) idx)

let max a b =
if a = b then a
else if a > b then a
else b



let rec  max_func_chain init funcs = 
match funcs with
[] -> init
| curr::rest ->
let applyCurr = curr init in
let max1 = max applyCurr (max_func_chain applyCurr rest ) in 
let max2 = (max_func_chain init rest) in 
max max1 max2
