open Pa

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

module ReceiveAfterSendNeg =
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
  let all = "/tmp/synpa/receive_after_send_neg",
            [queue1,  clusterize ;
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

