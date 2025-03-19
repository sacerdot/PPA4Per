type expr =
 | Var of string
 | Con of float
 | Sub of expr * expr
 | Add of expr * expr
 | Mul of expr * expr

let mul e1 e2 =
 match e1,e2 with
 | Con 1., e
 | e, Con 1. -> e
 | _, _ -> Mul(e1,e2)

let rec pp_expr fmt =
 function
  | Var s -> Format.fprintf fmt "%s" s
  | Con f -> Format.fprintf fmt "%g" f
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
    Printf.sprintf "  %s -> %s [label=\"%s\" fontsize=10]" st1 st2 ("<" ^ show_multiaction inp ^ "," ^ show_prob prob ^ "," ^ show_multiaction out ^ ">")

  let pp_state_trans (st, transitions) =
   String.concat "\n"
    (List.map (pp_transition st) transitions)

  let pp_clusters proc =
   function
      None -> ""
    | Some f ->
       let states1, states2 = List.partition f (List.map fst proc) in
       "subgraph { rank=same; " ^
        String.concat "; " states1 ^
        " }\n" ^
       "subgraph { rank=same; " ^
        String.concat "; " states2 ^
        " }\n"

  let pp_process (proc, clusterize) =
   "digraph {\n" ^
    pp_clusters proc clusterize ^
    String.concat "\n" (List.map pp_state_trans proc) ^
    "\n}\n"

  (* filepath is the path of a file without extension
     procs a list of processes
     The functions creates filepath.{gv,pdf} with one page for every process *)
  let dot_of_processes (filepath,procs) =
   let fd = open_out (filepath ^ ".gv") in
   List.iter (fun proc -> output_string fd (pp_process proc)) procs ;
   close_out fd ;
   ignore (Sys.command ("dot -Tps:cairo:cairo " ^ filepath ^ ".gv | ps2pdf - > " ^ filepath ^ ".pdf"))
 end

let opp (b, c) = (not b, c)

let cartesian l1 l2 =
 List.fold_right
  (fun x acc -> List.map (fun y -> x,y) l2 @ acc)
  l1 []

let incompatible _m1 _m2 =
 false (*M.exists (fun i -> M.exists (fun j -> j = opp i) m2) m1*)

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
           Some (inp,(mul eps1 eps2,mangle_states s1 s2,out)))
        (cartesian moves1 moves2)))
 (cartesian proc1 proc2)

let restrict (acts : action list) (proc : process) : process =
 List.map
  (fun (s, moves) ->
    s, List.filter
        (fun (inp,(_,_,out)) ->
          let badin = M.exists (fun a -> List.mem a acts) inp in
          let badout = M.exists (fun a -> List.mem a acts) out in
          if not badin && badout then
            prerr_endline "==> RENORMALIZE PROBABILITIES!" ;
          not (badin || badout))
       moves)
  proc

let reachable_from (proc : process) (st : state) : process =
 let rec aux visited =
  function
     [] -> visited
   | st::to_visit ->
      if List.exists (fun (st',_) -> st=st') visited then aux visited to_visit
      else
       let trans = List.assoc st proc in
       let visited = (st,trans)::visited in
       let to_visit = List.map (fun (_,(_,st,_)) -> st) trans @ to_visit in
       aux visited to_visit
 in
  aux [] [st]

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

  let queue1 = p (Var "p₁") (Var "q₁")
  let queue2 = q (Var "p₂") (Var "q₂")
  let queue3 = r (Var "p₃")
  let queue12 = parallel queue1 queue2
  let test = parallel queue12 queue3

  let all = "/tmp/synpa/two_feeders",
            [queue1,  None (*Some (fun _ -> true)*) ;
             queue2,  None (*Some (fun _ -> true)*) ;
             queue3,  None (*Some (fun _ -> true)*) ;
             queue12, None ;
             test,    None ]
end

module ReceiveAfterSend =
struct
  (* prcons = probability to decrement job number and send an a *)
  (* Send after receive, 3 states *)
  let mk_queue name capacity (a,na) (b,nb) prcons : process =
   let prncons = Sub(Con 1., prcons) in
   let p n = name ^ string_of_int n in
   let rec aux =
    function
     | 0 ->
        [ p 0, [ M.singleton a,  (Con 1.,  p 1, M.singleton nb)
               ; M.singleton na, (Con 1.,  p 0, M.singleton nb) ]]
     | n when n = capacity ->
        ( p n, [ M.singleton a,  (Con 1.,  p n, M.singleton nb)
               ; M.singleton na, (Con 1.,  p n, M.singleton nb) ])
        :: aux (n - 1)
     | n ->
        ( p n, [ M.singleton a,  (prncons,  p (n+1), M.singleton nb)
               ; M.singleton a,  (prcons,   p n,     M.singleton b)
               ; M.singleton na, (prcons ,  p (n-1), M.singleton b)
               ; M.singleton na, (prncons,  p n,     M.singleton nb) ])
        :: aux (n - 1) in
   aux capacity
  
  let a  = (true, 'a')
  let na = (false, 'a')
  let b  = (true, 'b')
  let nb = (false, 'b')
  
  let queue1 = mk_queue "P" 4 (a,na) (b,nb) (Var "p")
  let queue2 = mk_queue "Q" 4 (b,nb) (a,na) (Var "q")
  let test = restrict [a;na;b;nb] (parallel queue1 queue2)
  
  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/receive_after_send",
            [queue1,  clusterize ;
             queue2,  clusterize ;
             test,    clusterize ]
end

module ReceiveAfterSendPure =
struct
  let a  = (true, 'a')
  let b  = (true, 'b')

  (* Queue send after receive *)
  (* prcons = probability to decrement job number and send a b *)
  let mk_queue name capacity a b prcons : process =
   let prncons = Sub(Con 1., prcons) in
   let p n = name ^ string_of_int n in
   let rec aux =
    function
     | 0 ->
        [ p 0, [ M.singleton a,  (Con 1.,  p 1, M.empty)
               ; M.empty,        (Con 1.,  p 0, M.empty) ]]
     | n when n = capacity ->
        ( p n, [ M.singleton a,  (Con 1.,  p n, M.empty)
               ; M.empty,        (Con 1.,  p n, M.empty) ])
        :: aux (n - 1)
     | n ->
        ( p n, [ M.singleton a,  (prncons,  p (n+1), M.empty)
               ; M.singleton a,  (prcons,   p n,     M.singleton b)
               ; M.empty,        (prcons ,  p (n-1), M.singleton b)
               ; M.empty,        (prncons,  p n,     M.empty) ])
        :: aux (n - 1) in
   aux capacity

  let mk_source name a pr :  process =
   let npr = Sub(Con 1., pr) in
   [ name, [ M.empty, (pr,  name, M.singleton a)
           ; M.empty, (npr, name, M.empty)       ]]

  let source = mk_source "S" a (Var "s")
  let queue1 = mk_queue "P" 4 a b (Var "p")
  let queue2 = mk_queue "Q" 4 b a (Var "q")
  let test = restrict [a;b] (parallel source (parallel queue1 queue2))

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/receive_after_send_pure",
            [source,  clusterize ;
             queue1,  clusterize ;
             queue2,  clusterize ;
             test,    clusterize ]
end


module ExampleLoop =
struct
  let a  = (true, 'a')
  let na = (false, 'a')
  let b  = (true, 'b')
  let nb = (false, 'b')

  (* prcons = probability to decrement job number and send an a *)
  (* Send after receive, 3 states *)
  let p name (a,na) (b,nb) prcons : process =
   let p0 = name ^ "0" in
   let p1 = name ^ "1" in
   let p2 = name ^ "2" in
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

  let queue1 = p "P" (a,na) (b,nb) (Var "p")
  let queue2 = p "Q" (b,nb) (a,na) (Var "q")
  let test = parallel queue1 queue2

  let all = "/tmp/synpa/tandem_loop",
            [queue1,  Some (fun _ -> true) ;
             queue2,  Some (fun _ -> true) ;
             test,    Some (fun _ -> true) ]
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

  let test00 = reachable_from test "Q0_R0"
  let test10 = reachable_from test "Q1_R0"
  let test20 = reachable_from test "Q2_R0"

  let clusterize_on c s = s.[0] = c

  let all = "/tmp/synpa/tandem_loop_bipartite",
            [queue1, Some (clusterize_on 'P') ;
             queue2, Some (clusterize_on 'S') ;
             test00, Some (clusterize_on 'Q') ;
             test10, Some (clusterize_on 'Q') ;
             test20, Some (clusterize_on 'Q') ]
end
