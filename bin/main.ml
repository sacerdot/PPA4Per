open Synpa

let () =
  (*
  print_endline "========== Two queues feeding one queue ==========" ;
  print_endline (show_process Example.test) ;
  Dot.dot_of_processes Example.all ;
  print_endline "========== A tandem in loop ==========" ;
  print_endline (show_process ExampleLoop.test) ;
  Dot.dot_of_processes ExampleLoop.all ;
  print_endline "========== A tandem in loop (bipartite) ==========" ;
  print_endline (show_process ExampleLoopBi.test) ;
  Dot.dot_of_processes ExampleLoopBi.all ;
  *)
  print_endline "========== Receive after send ==========" ;
  print_endline (show_process ReceiveAfterSend.test) ;
  Dot.dot_of_processes ReceiveAfterSend.all ;
  print_endline "========== Receive after send pure ==========" ;
  print_endline (show_process ReceiveAfterSendPure.test) ;
  Dot.dot_of_processes ReceiveAfterSendPure.all ;
