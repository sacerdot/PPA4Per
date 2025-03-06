type expr =
 | Var of string
 | Con of float
 | Sub of expr * expr
 | Add of expr * expr
 | Mul of expr * expr

let rec pp_expr fmt =
 function
  | Var s -> Format.fprintf fmt "%s" s
  | Con f -> Format.fprintf fmt "%.2f" f
  | Sub(e1,e2) -> Format.fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
  | Add(e1,e2) -> Format.fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
  | Mul(e1,e2) -> Format.fprintf fmt "%a * %a" pp_expr e1 pp_expr e2

(* Matlab ha un formato per catene di Markov al tempo discreto *)

type action = bool * char (*[@@deriving show]*)  (* true = positive, false = negative *)
let pp_action fmt (b,c) = Format.fprintf fmt "%s%c" (if b then "" else "-") c

module M = (* set in place of multiset! *)
 struct
   include Set.Make(struct type t = action let compare = compare end)
   type elements = action list [@@deriving show]
   let pp fmt s = Format.fprintf fmt "%s" (show_elements (elements s))
 end
type multiaction = M.t [@@deriving show]
type state = string [@@deriving show]
type prob = expr [@@deriving show]
type process = (state * (multiaction * (prob * state * multiaction)) list) list [@@deriving show]

module Dot =
 struct
  let pp_transition st1 (inp, (prob, st2, out)) =
    Printf.sprintf "  %s -> %s [label=\"%s\"]" st1 st2 ("<" ^ show_multiaction inp ^ "," ^ show_prob prob ^ "," ^ show_multiaction out ^ ">")

  let pp_state_trans (st, transitions) =
   String.concat "\n"
    (List.map (pp_transition st) transitions)

  let pp_process proc =
   "digraph {\n" ^
    String.concat "\n" (List.map pp_state_trans proc) ^
    "\n}\n"
 end

let opp (b, c) = (not b, c)

let cartesian l1 l2 =
 List.fold_right
  (fun x acc -> List.map (fun y -> x,y) l2 @ acc)
  l1 []

let incompatible m1 m2 =
 M.exists (fun i -> M.exists (fun j -> j = opp i) m2) m1

(* Merge together transitions with same input set, output set and target state,
   adding the probabilities *)
let consolidate l =
 let l =
  List.sort (fun (inp1,(_,s1,out1)) (inp2,(_,s2,out2)) -> compare (inp1,s1,out1) (inp2,s2,out2)) l in
 let rec aux =
  function
     [] | [_] as l -> l
   | (inp1,(prob1,s1,out1))::(inp2,(prob2,s2,out2))::l when inp1=inp2 && s1=s2 && out1=out2 ->
       aux ((inp1,(Add(prob1,prob2),s1,out1))::l)
   | tran::l -> tran::aux l
 in
  aux l

let parallel (proc1 : process) (proc2 : process) : process =
 let mangle_states s1 s2 = s1 ^ "_" ^ s2 in
 List.map
 (fun ((s1,moves1),(s2,moves2)) ->
   mangle_states s1 s2,
     consolidate
      (List.filter_map
        (fun ((inp1,(eps1,s1,out1)),(inp2,(eps2,s2,out2))) ->
          if incompatible inp1 out2 || incompatible inp2 out1 then
           None
          else
           let inp = M.union inp1 inp2 in
           let out = M.union out1 out2 in
           let intersect = M.inter inp out in
           let inp = M.diff inp intersect in
           let out = M.diff out intersect in
           Some (inp,(Mul(eps1,eps2),mangle_states s1 s2,out)))
        (cartesian moves1 moves2)))
 (cartesian proc1 proc2)

module Example =
struct
  let a  = (true, 'a')
  let na = (false, 'a')
  let b  = (true, 'b')
  let nb = (false, 'b')

  (* prgen = probability to increment job number
     prcons = probability to decrement job number and send an a *)
  let p prgen prcons : process =
   let p0 = "p0" in
   let p1 = "p1" in
   let p2 = "p2" in
   let prngen  = Sub(Con 1., prgen) in
   let prncons = Sub(Con 1., prcons) in
   let eps00  = prngen in
   let eps01  = prgen in
   let eps11  = Mul(prngen,prncons) in
   let eps10  = Mul(prngen,prcons) in
   let eps11' = Mul(prgen, prcons) in
   let eps12  = Mul(prgen, prncons) in
   let eps22  = prncons in
   let eps21  = prcons in
   [ p0, [ M.empty, (eps00, p0, M.singleton na) ; M.empty, (eps01, p1, M.singleton na) ]
   ; p1, [ M.empty, (eps11, p1, M.singleton na) ; M.empty,  (eps10, p0, M.singleton a) ; M.empty, (eps11', p1, M.singleton a) ; M.empty, (eps12, p2, M.singleton na) ]
   ; p2, [ M.empty, (eps22, p2, M.singleton na) ; M.empty,  (eps21, p1, M.singleton a) ]
   ]

  (* prgen = probability to increment job number
     prcons = probability to decrement job number and send an a *)
  let q prgen prcons : process =
   let q0 = "q0" in
   let q1 = "q1" in
   let q2 = "q2" in
   let prngen  = Sub(Con 1., prgen) in
   let prncons = Sub(Con 1., prcons) in
   let eps00  = prngen in
   let eps01  = prgen in
   let eps11  = Mul(prngen,prncons) in
   let eps10  = Mul(prngen,prcons) in
   let eps11' = Mul(prgen, prcons) in
   let eps12  = Mul(prgen, prncons) in
   let eps22  = prncons in
   let eps21  = prcons in
   [ q0, [ M.empty, (eps00, q0, M.singleton nb) ; M.empty, (eps01, q1, M.singleton nb) ]
   ; q1, [ M.empty, (eps11, q1, M.singleton nb) ; M.empty,  (eps10, q0, M.singleton b) ; M.empty, (eps11', q1, M.singleton b) ; M.empty, (eps12, q2, M.singleton nb) ]
   ; q2, [ M.empty, (eps22, q2, M.singleton nb) ; M.empty,  (eps21, q1, M.singleton b) ]
   ]

  (* pra = probability to receive a
     prb = probability to receive b
     prcons = probability to decrement job number *)
  let r prcons : process =
   let r0 = "r0" in
   let r1 = "r1" in
   let r2 = "r2" in
   let prncons = Sub(Con 1., prcons) in
   let eps00   = Con 1. in
   let eps00'  = prcons in
   let eps00'' = prcons in
   let eps01   = prncons in
   let eps01'  = prncons in
   let eps01'' = prcons in
   let eps02   = prncons in
   let eps10   = prcons in
   let eps11   = prncons in
   let eps11'  = prcons in
   let eps11'' = prcons in
   let eps12   = prncons in
   let eps12'  = prncons in
   let eps22   = prncons in
   let eps21   = prcons in
   [ r0, [ M.of_list [na;nb], (eps00, r0, M.empty) ; M.of_list [a;nb], (eps01, r1, M.empty) ; M.of_list [a;nb], (eps00', r0, M.empty) ;
           M.of_list [na;b],  (eps01', r1, M.empty) ; M.of_list [na;b], (eps00'', r0, M.empty) ;
           M.of_list [a;b],   (eps02, r2, M.empty) ; M.of_list [a;b], (eps01'', r1, M.empty) ]
   ; r1, [ M.of_list [na;nb], (eps11, r1, M.empty) ; M.of_list [na;nb],  (eps10, r0, M.empty) ; M.of_list [a;nb], (eps12, r2, M.empty) ; M.of_list [na;b], (eps12', r2, M.empty) ; M.of_list [a;nb], (eps11',r1, M.empty) ;
           M.of_list [na;b], (eps11'', r1, M.empty)] (* what about [a;b] ??? *)
   ; r2, [ M.of_list [na;nb], (eps22, r2, M.empty) ; M.of_list [na;nb],  (eps21, r1, M.empty) ]
     (* what about [a;b|nb] and [a|na;b] *)
   ]

  let test = parallel (parallel (p (Var "p₁") (Var "q₁")) (q (Var "p₂") (Var "q₂"))) (r (Var "p₃"))
end

module ExampleLoop =
struct
  let a  = (true, 'a')
  let na = (false, 'a')
  let b  = (true, 'b')
  let nb = (false, 'b')

  (* prcons = probability to decrement job number and send an a *)
  let p prcons : process =
   let p0 = "p0" in
   let p1 = "p1" in
   let p2 = "p2" in
   let prncons = Sub(Con 1., prcons) in

   let eps00  = prcons in
   let eps00' = Con 1. in
   let eps01  = prncons in

   let eps11  = prcons in
   let eps12  = prncons in
   let eps10  = prcons in
   let eps11' = prncons in

   let eps22  = Con 1. in

   [ p0, [ M.singleton a,  (eps01,  p1, M.singleton nb)
         ; M.singleton a,  (eps00,  p0, M.singleton b)
         ; M.singleton na, (eps00', p0, M.singleton nb) ]
   ; p1, [ M.singleton a,  (eps12,  p2, M.singleton nb)
         ; M.singleton a,  (eps11,  p1, M.singleton b)
         ; M.singleton na, (eps10,  p0, M.singleton b)
         ; M.singleton na, (eps11', p1, M.singleton nb) ]
   ; p2, [ M.singleton a,  (eps22,  p2, M.singleton nb)
         ; M.singleton na, (eps22,  p2, M.singleton nb) ]
   ]

  (* prcons = probability to decrement job number and send an a *)
  let q prcons : process =
   let q0 = "q0" in
   let q1 = "q1" in
   let q2 = "q2" in
   let prncons = Sub(Con 1., prcons) in

   let eps00  = prcons in
   let eps01  = prncons in
   let eps00' = Con 1. in

   let eps11  = prcons in
   let eps12  = prncons in
   let eps10  = prcons in
   let eps11' = prncons in

   let eps22  = Con 1. in

   [ q0, [ M.singleton b,  (eps01,  q1, M.singleton na)
         ; M.singleton b,  (eps00,  q0, M.singleton a)
         ; M.singleton nb, (eps00', q0, M.singleton na) ]
   ; q1, [ M.singleton b,  (eps12,  q2, M.singleton na)
         ; M.singleton b,  (eps11,  q1, M.singleton a)
         ; M.singleton nb, (eps10,  q0, M.singleton a)
         ; M.singleton nb, (eps11', q1, M.singleton na) ]
   ; q2, [ M.singleton b,  (eps22,  q2, M.singleton na)
         ; M.singleton nb, (eps22,  q2, M.singleton na) ]
   ]

  let test = parallel (p (Var "p")) (q (Var "q"))
end

module ExampleLoopBi =
struct
  let a  = (true, 'a')
  let na = (false, 'a')
  let b  = (true, 'b')
  let nb = (false, 'b')

  (* prcons = probability to decrement job number and send an a *)
  let p name1 name2 (a,na) (b,nb) prcons : process =
   let p0 = name1 ^ "0" in
   let p1 = name1 ^ "1" in
   let p2 = name1 ^ "2" in
   let p3 = name1 ^ "3" in
   let q0 = name2 ^ "0" in
   let q1 = name2 ^ "1" in
   let q2 = name2 ^ "2" in
   let q3 = name2 ^ "3" in
   let prncons = Sub(Con 1., prcons) in

   [ p0, [ M.singleton a,  (Con 1.,  q1, M.empty)
         ; M.singleton na, (Con 1.,  q0, M.empty)]
   ; q0, [ M.empty,        (Con 1.,  p0, M.singleton nb)]
   ; p1, [ M.singleton a,  (Con 1.,  q2, M.empty)
         ; M.singleton na, (Con 1.,  q1, M.empty)]
   ; q1, [ M.empty,        (prcons,  p0, M.singleton b)
         ; M.empty,        (prncons, p1, M.singleton nb)]
   ; p2, [ M.singleton a,  (Con 1.,  q3, M.empty)
         ; M.singleton na, (Con 1.,  q2, M.empty)]
   ; q2, [ M.empty,        (prcons,  p1, M.singleton b)
         ; M.empty,        (prncons, p2, M.singleton nb)]
   ; p3, [ M.singleton a,  (Con 1.,  q3, M.empty)
         ; M.singleton na, (Con 1.,  q3, M.empty)]
   ; q3, [ M.empty,        (Con 1.,  p3, M.singleton nb)]
   ]

  let queue1 = p "P" "Q" (a,na) (b,nb) (Var "p")
  let queue2 = p "R" "S" (b,nb) (a,na) (Var "q")
  let test = parallel queue1 queue2
end
