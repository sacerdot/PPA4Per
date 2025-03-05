type expr =
 | Var of string
 | Con of float
 | Min of expr * expr
 | Mul of expr * expr

let rec pp_expr fmt =
 function
  | Var s -> Format.fprintf fmt "%s" s
  | Con f -> Format.fprintf fmt "%.2f" f
  | Min(e1,e2) -> Format.fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
  | Mul(e1,e2) -> Format.fprintf fmt "%a * %a" pp_expr e1 pp_expr e2

(* Matlab ha un formato per catene di Markov al tempo discreto *)

type action = char [@@deriving show]
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

let cartesian l1 l2 =
 List.fold_right
  (fun x acc -> List.map (fun y -> x,y) l2 @ acc)
  l1 []

let parallel (proc1 : process) (proc2 : process) =
 let mangle_states s1 s2 = s1 ^ "_" ^ s2 in
 List.map
 (fun ((s1,moves1),(s2,moves2)) ->
   mangle_states s1 s2,
     List.map
     (fun ((acts1,(eps1,s1,out1)),(acts2,(eps2,s2,out2))) ->
       let ins = M.union acts1 acts2 in
       let outs = M.union out1 out2 in
       let intersect = M.inter ins outs in
       let ins = M.diff ins intersect in
       let outs = M.diff outs intersect in
       ins,(Mul(eps1,eps2),mangle_states s1 s2,outs))
     (cartesian moves1 moves2))
 (cartesian proc1 proc2)

module Example =
struct
  let a  = 'a'
  let na = 'A'
  let b  = 'b'
  let nb = 'B'

  (* prgen = probability to increment job number
     prcons = probability to decrement job number and send an a *)
  let p prgen prcons : process =
   let p0 = "p0" in
   let p1 = "p1" in
   let p2 = "p2" in
   let prngen  = Min(Con 1., prgen) in
   let prncons = Min(Con 1., prcons) in
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
   let prngen  = Min(Con 1., prgen) in
   let prncons = Min(Con 1., prcons) in
   let eps00  = prngen in
   let eps01  = prgen in
   let eps11  = Mul(prngen,prncons) in
   let eps10  = Mul(prngen,prcons) in
   let eps11' = Mul(prgen, prcons) in
   let eps12  = Mul(prgen, prncons) in
   let eps22  = prncons in
   let eps21  = prcons in
   [ q0, [ M.empty, (eps00, q0, M.singleton nb) ; M.empty, (eps01, q1, M.singleton nb) ]
   ; q1, [ M.empty, (eps11, q1, M.singleton nb) ; M.empty,  (eps10, q0, M.singleton b) ; M.empty, (eps11', q1, M.singleton a) ; M.empty, (eps12, q2, M.singleton na) ]
   ; q2, [ M.empty, (eps22, q2, M.singleton nb) ; M.empty,  (eps21, q1, M.singleton b) ]
   ]

  (* pra = probability to receive a
     prb = probability to receive b
     prcons = probability to decrement job number *)
  let r prcons : process =
   let r0 = "r0" in
   let r1 = "r1" in
   let r2 = "r2" in
   let prncons = Min(Con 1., prcons) in
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
