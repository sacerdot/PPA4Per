type expr =
 | Var of string
 | Con of float
 | Compl of expr  (* 1 - e *)
 | Add of expr * expr
 | Mul of expr * expr

let rec mul e1 e2 =
 match e1,e2 with
 | Con 1., e
 | e, Con 1. -> e
 | Con x, Con y -> Con (x *. y)
 | Mul(e11,e12), e -> mul e11 (mul e12 e)
 | _, _ -> Mul(e1,e2)

let rec add e1 e2 =
 match e1,e2 with
 | Con 0., e
 | e, Con 0. -> e
 | Con x, Con y -> Con (x +. y)
 | Add(e11,e12), e -> add e11 (add e12 e)
 | _, _ -> Add(e1,e2)

let distr l1 l2 =
 List.concat (List.map (fun x -> List.map (fun y -> mul x y) l2) l1)

let compl_aux =
 function
  | Var _ | Mul _ as x -> Compl x
  | Con f -> Con (1. -. f)
  | Compl e -> e
  | Add _ -> assert false

let rec add_mul_nf =
 function
  | Var _ | Con _ as e -> [e]
  | Compl e -> List.map compl_aux (add_mul_nf e)
  | Add(e1,e2) -> add_mul_nf e1 @ add_mul_nf e2
  | Mul(e1,e2) -> distr (add_mul_nf e1) (add_mul_nf e2)

let simpl e =
 let rec aux =
  function
   | [] -> assert false
   | [x] -> x
   | x::l -> add x (aux l)
 in
  aux (add_mul_nf e)

let priority =
 function
  | Var _ | Con _ -> 3
  | Mul _ -> 3
  | Add _ -> 2
  | Compl _ -> 1

let rec pp_expr ?(parens=false) fmt expr =
 if parens then Format.fprintf fmt "(" ;
 let p = priority expr in
 (match expr with
  | Var s -> Format.fprintf fmt "%s" s
  | Con f -> Format.fprintf fmt "%g" f
  | Compl e -> Format.fprintf fmt "1-%a" (pp_expr ~parens:(priority e <= p)) e
  | Add(e1,e2) -> Format.fprintf fmt "%a+%a" (pp_expr ~parens:(priority e1 < p)) e1 (pp_expr ~parens:(priority e2 < p)) e2
  | Mul(e1,e2) -> Format.fprintf fmt "%a%a" (pp_expr ~parens:(priority e1 < p)) e1 (pp_expr ~parens:(priority e2 < p)) e2) ;
 if parens then Format.fprintf fmt ")"

let pp_expr fmt expr =
 pp_expr fmt (simpl expr)

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
        (fun ((inp,(_,_,out)) as action) ->
          let badin = M.exists (fun a -> List.mem a acts) inp in
          let badout = M.exists (fun a -> List.mem a acts) out in
          if not badin && badout then begin
            ignore action (*Format.eprintf "==> RENORMALIZE PROBABILITY: %a\n" pp_process [s, [action]]*)
          end ;
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

exception NotAMarkovChain of string

(* Output in Matlab/Octave syntax *)

let markov_of_process (proc : process) strnames strmatr =
 let names = List.rev_map (fun (st,_) -> st) proc in
 let matrix =
  List.rev_map
   (fun (_,moves) ->
     let probs =
      List.map
       (fun st ->
         List.fold_left
          (fun acc (ins, (e, st', outs)) ->
            let e' = simpl e in
            match e' with
             | Con p when st=st' ->
                if M.is_empty ins && M.is_empty outs then
                 acc +. p
                else
                raise (NotAMarkovChain "not isolated")
             | _ when st=st' ->
                raise (NotAMarkovChain (Format.asprintf "symbolic probabilities: %a" pp_expr e'))
             | _ -> acc)
         0.0 moves)
       names in
     let sum = List.fold_left (+.) 0.0 probs in
     if abs_float (sum -. 1.0) > 0.0001 then
      raise (NotAMarkovChain (Format.sprintf "probs add to %f" sum))
     else
      probs)
   proc in
 strnames ^ "={" ^ String.concat "," (List.map (fun n -> "\"" ^ n ^ "\"") names) ^ "}\n" ^
 strmatr  ^ "=[" ^ String.concat ";" (List.map (fun r -> String.concat "," (List.map string_of_float r)) matrix) ^ "]"
