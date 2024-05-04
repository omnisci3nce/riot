[@@@warning "-8"]

open Riot

type Message.t += AnswerToAllTheWorldsProblems of int
type Message.t += ProofOfLife
type Message.t += A | B | C

let string_of_msg = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | _ -> "Unknown"

let rec loop pid msg n =
  let s = Format.sprintf "Loop pid %s with n = %d" (string_of_msg msg) n in
  Logger.info (fun f -> f "%s" s);

  if n = 0 then send pid msg
  else (
    yield ();
    loop pid msg (n - 1))

let count_to_one_million pid () =
  let rec fac n acc =
    Logger.info (fun f -> f "Factorial %d" n);
    flush_all ();
    (* sleep 0.1; *)
    if n == 0 then acc else fac (n - 1) (acc * n)
  in

  let number = fac 30 1 in
  send pid (AnswerToAllTheWorldsProblems number)

let rec wait_for_answer () =
  Printf.printf "RECV\n";
  match receive () with
  | AnswerToAllTheWorldsProblems n ->
      Printf.printf
        "Got the answer!\n\
        \ The answer to all the worlds problems has been calculated to be %d\n"
        n
  (* shutdown () *)
  | ProofOfLife ->
      (* print_endline "Waiting"; *)
      wait_for_answer ()

let () =
  Riot.run ~workers:0 @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Debug);

  let this = self () in

  let pid_waiting = spawn wait_for_answer in
  let _pid_long_running = spawn_blocking (count_to_one_million pid_waiting) in
  send pid_waiting ProofOfLife;
  let _ =
    spawn (fun () ->
        process_flag (Priority Low);
        loop this A 100)
  in
  let _ =
    spawn (fun () ->
        process_flag (Priority Normal);
        loop this B 100)
  in
  let _pid_c =
    spawn (fun () ->
        process_flag (Priority High);
        loop this C 100)
  in
  (* let m1 = receive ~after:50_000L () in
     let m2 = receive ~after:50_000L () in
     let m3 = receive ~after:50_000L () in

     match (m1, m2, m3) with
     | C, B, A ->
         Logger.info (fun f -> f "process_priority_test: OK");

         (* shutdown () *)
     | m1, m2, m3 ->
         Logger.error (fun f ->
             f "process_priority_test: messages arrived out of order?\n%S\n%S\n%S"
               (Marshal.to_string m1 []) (Marshal.to_string m2 [])
               (Marshal.to_string m3 []));
         Printf.eprintf "fak";
         ()
         ; *)
  wait_pids [ pid_waiting ];
  flush_all ();
  shutdown ()
