open Synpa
open Pa

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
  print_endline "========== Receive after send (negative actions) ==========" ;
  print_endline (show_process ReceiveAfterSendNeg.test) ;
  Dot.dot_of_processes ReceiveAfterSendNeg.all ;
  *)
  (*
  print_endline "========== RAS: One Source One Tandem ==========" ;
  print_endline (show_process ReceiveAfterSend.OneSourceOneTandem.test) ;
  Dot.dot_of_processes ReceiveAfterSend.OneSourceOneTandem.all ;
  *)
  (*
  print_endline "========== RAS: Two To One ==========" ;
  let module M = ReceiveAfterSend.TwoToOne() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  *)
  (*
  print_endline "========== RAS: Source Queue ==========" ;
  let module M = ReceiveAfterSend.SourceQueue() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  *)
  (*
  print_endline "========== SAR: Source Queue ==========" ;
  let module M = SendAfterReceive.SourceQueue() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  *)
  print_endline "========== SAR: Tandem ==========" ;
  let module M = SendAfterReceive.Tandem() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  (*
  print_endline "========== RAS: Tandem ==========" ;
  let module M = ReceiveAfterSend.Tandem() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  *)
  (*
  print_endline "========== RAS: TwoSourcesOneQueueMerge ==========" ;
  let module M = ReceiveAfterSend.TwoSourcesOneQueueMerge() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  *)
  (*
  print_endline "========== RAS: (ground) Tandem ==========" ;
  let module M = ReceiveAfterSend.GroundTandem() in
  print_endline (show_process M.test) ;
  Dot.dot_of_processes M.all ;
  print_endline (markov_of_process M.test "states" "M")
*)
