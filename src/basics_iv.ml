open Funs
(*****************)
(* Part 4: HOF *)
(*****************)

let targCheck targ elem =
if targ = elem then true
else false

let is_there lst x = fold (fun a h -> 
 if a then true
 else ((targCheck x) h)
) false lst



let targBonus targ elem = 
if targ = elem then 1
else 0


let count_occ lst target = fold_right (fun h a ->
 ((targBonus target) h) + a
) lst 0






let uniq lst = fold_right (fun h a ->
if (is_there a h) then a
else h::a
) lst []

let modCheck targ num = 
if num mod targ = 0 then true
else false



let indexTup lst = fold (fun (idx,tup) elem  -> ((idx+1,(idx,elem)::tup) ) ) (1,[]) lst


let rmvCountTup lst = 
match lst with
(num, lst) -> lst 



let every_xth x lst = 
let tupList = rmvCountTup (indexTup lst) in
fold (fun a h ->
match h with
(idx,elem) -> if ((modCheck x) idx)  then elem::a else a
)
[] tupList  

