open Pa

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
