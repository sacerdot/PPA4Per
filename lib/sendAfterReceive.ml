open Pa

let mk_buffer name a b :  process =
 let p n = name ^ string_of_int n in
 [ p 0, [ M.empty,       (Con 1.,  p 0, M.empty)
        ; M.singleton a, (Con 1.,  p 1, M.empty)       ]
 ; p 1, [ M.empty,       (Con 1.,  p 0, M.singleton b)
        ; M.singleton a, (Con 1.,  p 1, M.singleton b) ]]

let mk_source name a pr :  process =
 let npr = Compl pr in
 [ name, [ M.empty, (pr,  name, M.singleton a)
         ; M.empty, (npr, name, M.empty)       ]]

(* Queue send (to b) after receive (from a) *)
(* prcons = probability to decrement job number and send *)
let mk_queue name capacity a b prcons : process =
 let prncons = Compl prcons in
 let p n = name ^ string_of_int n in
 let rec aux =
  function
   | 0 ->
      [ p 0, [ M.singleton a,  (prncons, p 1, M.empty)
             ; M.singleton a,  (prcons,  p 0, M.singleton b)
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
(* When full it accepts everything and emits nothing *)
let mk_double_queue name capacity a b c prcons : process =
 let prncons = Compl prcons in
 let p n = name ^ string_of_int n in
 let rec aux =
  function
   | 0 ->
      [ p 0, [ M.singleton a,   (prncons,  p 1, M.empty)
             ; M.singleton b,   (prncons,  p 1, M.empty)
             ; M.of_list [a;b], (prncons,  p 2, M.empty)
             ; M.singleton a,   (prcons,   p 0, M.singleton c)
             ; M.singleton b,   (prcons,   p 0, M.singleton c)
             ; M.of_list [a;b], (prcons,   p 1, M.singleton c)
             ; M.empty,         (Con 1.,   p 0, M.empty) ]]
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

(* Queue send (to c) after receive (from a, b or both) *)
(* prcons = probability to decrement job number and send *)
(* When full it does not accept anymore *)
let mk_full_double_queue name capacity a b c prcons : process =
 let prncons = Compl prcons in
 let p n = name ^ string_of_int n in
 let rec aux =
  function
   | 0 ->
      [ p 0, [ M.singleton a,   (prncons,  p 1, M.empty)
             ; M.singleton b,   (prncons,  p 1, M.empty)
             ; M.of_list [a;b], (prncons,  p 2, M.empty)
             ; M.singleton a,   (prcons,   p 0, M.singleton c)
             ; M.singleton b,   (prcons,   p 0, M.singleton c)
             ; M.of_list [a;b], (prcons,   p 1, M.singleton c)
             ; M.empty,         (Con 1.,   p 0, M.empty) ]]
   | n when n = capacity - 1 ->
      ( p n, [ M.singleton a,   (Con 1.,  p (n+1), M.empty)
             ; M.singleton b,   (Con 1.,  p (n+1), M.empty)
             ; M.empty,         (Con 1.,  p n, M.empty) ])
      :: aux (n - 1)
   | n when n = capacity ->
      ( p n, [ M.empty,         (Con 1.,  p n, M.empty) ])
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


module SourceQueue() =
 struct
  let a  = 'a'
  let b  = 'b'
  let c  = 'c'

  let source = mk_source "S" a (Var "s")
  let queue = mk_queue "Q" 4 a b (Var "q")
  let buffer = mk_buffer "B" b c
  let par = parallel (parallel source queue) buffer
  let test = restrict [a;b] par

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/SAR_source_queue",
            [source,  clusterize ;
             queue,   clusterize ;
             buffer,  clusterize ;
             test,    clusterize ]
 end

module Tandem() =
 struct
  let a  = 'a'
  let a' = 'A'
  let b  = 'b'
  let b' = 'B'

  let queue1 = mk_queue "P" 4 a' b (Var "p")
  let buffer1 = mk_buffer "B₁" b b'
  let queue2 = mk_queue "Q" 4 b' a (Var "q")
  let buffer2 = mk_buffer "B₂" a a'
  let test = restrict [a;b;a';b'] (parallel (parallel queue1 buffer1) (parallel queue2 buffer2))

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/SAR_tandem",
            [queue1,  clusterize ;
             buffer1, clusterize ;
             queue2,  clusterize ;
             buffer2, clusterize ;
             test,    clusterize ]
 end


(* add buffers
module OneSourceOneTandem() =
 struct
  let a  = 'a'
  let b  = 'b'

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

module TwoToOne() =
 struct
  let x  = 'x'
  let y  = 'y'
  let a  = 'a'
  let b  = 'b'
  let c  = 'c'

  let source1 = mk_source "S1" x (Var "s1")
  let source2 = mk_source "S2" y (Var "s2")
  let queue1 = mk_queue "P" 4 x a (Var "p")
  let queue2 = mk_queue "Q" 4 y b (Var "q")
  let queue3 = mk_double_queue "R" 4 a b c (Var "r")
  let test = restrict [a;b;x;y] (parallel (parallel (parallel source1 queue1) (parallel source2 queue2)) queue3)

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/RAS_two_to_one",
            [source1,  clusterize ;
             source2,  clusterize ;
             queue1,  clusterize ;
             queue2,  clusterize ;
             queue3,  clusterize ;
             test,    clusterize ]
 end

module TwoSourcesOneQueueMerge() =
 struct
  let x  = 'x'
  let y  = 'y'
  let a  = 'a'
  let b  = 'b'
  let c  = 'c'

  let source1 = mk_source "S1" x (Var "s1")
  let source2 = mk_source "S2" y (Var "s2")
  let queue1 = mk_queue "P" 4 x a (Var "p")
  let queue3 = mk_full_double_queue "R" 4 a y c (Var "r")
  let test = restrict [a;x;y] (parallel (parallel (parallel source1 queue1) source2) queue3)

  let clusterize = None (*Some (fun _ -> true)*)
  let all = "/tmp/synpa/SAR_two_sources_one_queue_merge",
            [source1,  clusterize ;
             source2,  clusterize ;
             queue1,  clusterize ;
             queue3,  clusterize ;
             test,    clusterize ]
 end
*)
