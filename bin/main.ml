open Synpa

let () =
  print_endline "========== Two queues feeding one queue ==========" ;
  print_endline (show_process Example.test) ;
  print_endline "========== A tandem in loop ==========" ;
  print_endline (show_process ExampleLoop.test) ;
  print_endline "========== A tandem in loop (bipartite) ==========" ;
  print_endline (show_process ExampleLoopBi.test)
