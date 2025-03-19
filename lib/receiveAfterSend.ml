open Pa

let mk_source name a pr :  process =
 let npr = Sub(Con 1., pr) in
 [ name, [ M.empty, (pr,  name, M.singleton a)
         ; M.empty, (npr, name, M.empty)       ]]

(* Queue send (to b) after receive (from a) *)
(* prcons = probability to decrement job number and send *)
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

(* Queue send (to c) after receive (from a, b or both) *)
(* prcons = probability to decrement job number and send *)
let mk_double_queue name capacity a b c prcons : process =
 let prncons = Sub(Con 1., prcons) in
 let p n = name ^ string_of_int n in
 let rec aux =
  function
   | 0 ->
      [ p 0, [ M.singleton a,   (Con 1.,  p 1, M.empty)
             ; M.singleton b,   (Con 1.,  p 1, M.empty)
             ; M.of_list [a;b], (Con 1.,  p 2, M.empty)
             ; M.empty,         (Con 1.,  p 0, M.empty) ]]
   | n when n = capacity - 1 ->
      ( p n, [ M.singleton a,   (Con 1.,  p (n+1), M.empty)
             ; M.singleton b,   (Con 1.,  p (n+1), M.empty)
             ; M.of_list [a;b], (Con 1.,  p (n+1), M.empty)
             ; M.empty,         (Con 1.,  p n, M.empty) ])
      :: aux (n - 1)
   | n when n = capacity ->
      ( p n, [ M.singleton a,   (Con 1.,  p n, M.empty)
             ; M.singleton b,   (Con 1.,  p n, M.empty)
             ; M.of_list [a;b], (Con 1.,  p n, M.empty)
             ; M.empty,         (Con 1.,  p n, M.empty) ])
      :: aux (n - 1)
   | n ->
      ( p n, [ M.singleton a,   (prncons,  p (n+1), M.empty)
             ; M.singleton b,   (prncons,  p (n+1), M.empty)
             ; M.of_list [a;b], (prncons,  p (n+2), M.empty)
             ; M.singleton a,   (prcons,   p n,     M.singleton c)
             ; M.singleton b,   (prcons,   p n,     M.singleton c)
             ; M.of_list [a;b], (prcons,   p (n+1), M.singleton c)
             ; M.empty,         (prcons ,  p (n-1), M.singleton c)
             ; M.empty,         (prncons,  p n,     M.empty) ])
      :: aux (n - 1) in
 aux capacity

module OneSourceOneTandem =
 struct
  let a  = (true, 'a')
  let b  = (true, 'b')

  let source = mk_source "S" a (Var "s")
  let queue1 = mk_queue "P" 4 a b (Var "p")
  let queue2 = mk_queue "Q" 4 b a (Var "q")
  let test = restrict [a;b] (parallel source (parallel queue1 queue2))

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/ras_one_source_one_tandem",
            [source,  clusterize ;
             queue1,  clusterize ;
             queue2,  clusterize ;
             test,    clusterize ]
 end

module TwoToOne =
 struct
  let x  = (true, 'x')
  let y  = (true, 'y')
  let a  = (true, 'a')
  let b  = (true, 'b')
  let c  = (true, 'c')

  let queue1 = mk_queue "P" 4 x a (Var "p")
  let queue2 = mk_queue "Q" 4 y b (Var "q")
  let queue3 = mk_double_queue "R" 4 a b c (Var "r")
  let test = restrict [a;b] (parallel queue3 (parallel queue1 queue2))

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/ras_two_to_one",
            [queue1,  clusterize ;
             queue2,  clusterize ;
             queue3,  clusterize ;
             test,    clusterize ]
 end
