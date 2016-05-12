module Machine = struct

  type 'a result =
    | Completed of 'a list * int
    | Aborted of string
  ;;

  let null_writer = fun _ -> ();;

  let run ~program ~inputs ~memory ~log_writer =
    let the_end = Array.length program in
    let in_prog_range addr fn =
      if addr < 0 || addr >= the_end
      then Aborted(Printf.sprintf "Program address out of range: %d" addr)
      else fn addr
    in
    let in_mem_range addr fn =
      if addr < 0 || addr >= Array.length memory
      then Aborted(Printf.sprintf "Memory address out of range: %d" addr)
      else fn addr
    in
    let read_mem fn addr =
      match memory.(addr) with
      | None -> Aborted(Printf.sprintf "Memory empty at address %d" addr)
      | Some data -> fn data
    in
    let write_mem data fn addr =
      match data with
      | None -> Aborted("Register is empty")
      | Some _ -> memory.(addr) <- data; fn ()
    in

    let rec loop steps pc register inputs outputs =
      let no_more_input = (List.length inputs) = 0 in
      let at_the_end = (pc = the_end) in
      let log msg = log_writer (Printf.sprintf "[%d:%d] %s" steps pc msg) in
      let read_reg fn =
        match register with
        | None -> Aborted("Empty register")
        | Some data -> fn data
      in

      if at_the_end then
        if no_more_input then Completed(List.rev outputs, steps)
        else Aborted(Printf.sprintf "Not all input is processed. Items left: %d" (List.length inputs))
      else
        begin match program.(pc) with
        | `Input ->
          log "Input";
          begin match inputs with
          | [] -> Completed(List.rev outputs, steps + 1)
          | data :: inputs -> loop (steps + 1) (pc + 1) (Some(data)) inputs outputs
          end
        | `Output ->
          log "Output";
          begin match register with
          | None -> Aborted("Output: Empty output not allowed")
          | Some data -> loop (steps + 1) (pc + 1) None inputs (data :: outputs)
          end
        | `Jump addr ->
          log (Printf.sprintf "Jump: address %d" addr);
          in_prog_range addr (fun addr -> loop (steps + 1) addr register inputs outputs)
        | `JumpIfZero addr ->
          log (Printf.sprintf "JumpIfZero: address %d" addr);
          in_prog_range addr (fun addr ->
            read_reg (fun data -> loop (steps + 1) (if data = 0 then addr else (pc + 1)) register inputs outputs)
          )
        | `JumpIfNegative addr ->
          log (Printf.sprintf "JumpIfNegative: address %d" addr);
          in_prog_range addr (fun addr ->
            read_reg (fun data -> loop (steps + 1) (if data < 0 then addr else (pc + 1)) register inputs outputs)
          )
        | `CopyTo addr ->
          log (Printf.sprintf "CopyTo: address %d" addr);
          in_mem_range addr (write_mem register (fun () -> loop (steps + 1) (pc + 1) register inputs outputs))
        | `CopyFrom addr ->
          log (Printf.sprintf "CopyFrom: address %d" addr);
          in_mem_range addr (read_mem (fun data -> loop (steps + 1) (pc + 1) (Some(data)) inputs outputs))
        | `Add addr ->
          log (Printf.sprintf "Add: address %d" addr);
          in_mem_range addr (read_mem (fun data ->
            read_reg (fun reg -> loop (steps + 1) (pc + 1) (Some(reg + data)) inputs outputs))
          )
        | `Sub addr ->
          log (Printf.sprintf "Sub: address %d" addr);
          in_mem_range addr (read_mem (fun data ->
            read_reg (fun reg -> loop (steps + 1) (pc + 1) (Some(reg - data)) inputs outputs))
          )
        end
    in
    loop 0 0 None inputs []
  ;;

  module Tests = struct
    exception Failure of string;;
    type ('a, 'b) computer = { program: 'a array; memory: 'b array };;

    let describe text fn =
      Printf.printf "\n--- %s ---\n" text;
      fn ();
    ;;

    let case text fn =
      try
        fn ();
        Printf.printf "[ ok ] %s\n" text
      with
      | Failure reason -> Printf.printf "[fail] %s :: %s\n" text reason
      | e -> Printf.printf "[fail] %s :: %s\n" text (Printexc.to_string e)
    ;;

    let expect program =
      { program = program; memory = [||] }
    ;;

    let with_memory mem comp =
      { comp with memory = mem }
    ;;

    let when_run_with input comp =
      run ~program:comp.program ~inputs:input ~memory:comp.memory ~log_writer:null_writer
    ;;

    let to_complete res =
      match res with
      | Aborted reason -> raise (Failure(reason))
      | Completed _ -> res
    ;;

    let and_output expected_output res =
      match res with
      | Aborted reason -> raise (Failure(reason))
      | Completed (actual_output, _) ->
        List.combine expected_output actual_output
        |> List.iter (fun (e, a) ->
          if a != e then raise (Failure("output did not match"))
        );
        res
    ;;

    let within expected_steps res =
      match res with
      | Aborted reason -> raise (Failure(reason))
      | Completed (_, actual_steps) ->
        if actual_steps != expected_steps
        then raise (Failure(Printf.sprintf "expected %d steps, got %d" expected_steps actual_steps))
        else res
    ;;

    let to_abort res =
      match res with
      | Aborted reason -> res
      | Completed _ -> raise (Failure("expected to abort"))
    ;;

    (* ------------------------------------------------------------------ *)
    let run () =
      describe "simple" (fun () ->
        case "in/out" (fun () ->
          expect [|`Input;`Output|]
          |> when_run_with [1]
          |> to_complete
          |> and_output [1]
          |> within 2
        );

        case "loop" (fun () ->
          expect [|`Input;`Output;`Jump(0)|]
          |> when_run_with [1;2]
          |> to_complete
          |> and_output [1;2]
          |> within 7
        );
      );

      describe "program end" (fun () ->
        case "missing output" (fun () ->
          expect [|`Output|] |> when_run_with [1] |> to_abort
        );

        case "last input read" (fun () ->
          expect [|`Input;`Input;`Output|] |> when_run_with [1] |> to_complete |> and_output [] |> within 2
        );
      );

      describe "branching" (fun () ->
        case "jump if zero" (fun () ->
          expect [|`Input;`JumpIfZero(0);`Output;`Jump(0)|]
          |> when_run_with [0;1;0;2;0]
          |> to_complete
          |> and_output [1;2]
        );

        case "jump if negative" (fun () ->
          expect [|`Input;`JumpIfNegative(0);`Output;`Jump(0)|]
          |> when_run_with [0;1;-1;2;-2]
          |> to_complete
          |> and_output [0;1;2]
        );
      );

      describe "intermediate" (fun () ->
        case "memory" (fun () ->
          expect [|`Input;`CopyTo(0);`CopyFrom(0);`Output|]
          |> with_memory [|None|]
          |> when_run_with [1]
          |> to_complete
          |> and_output [1]
        );

        case "adding" (fun () ->
          expect [|`Input;`CopyTo(0);`Add(0);`Output|]
          |> with_memory [|None|]
          |> when_run_with [1]
          |> to_complete
          |> and_output [2]
        );

        case "subtracting" (fun () ->
          expect [|`Input;`CopyTo(0);`Input;`Sub(0);`Output|]
          |> with_memory [|None|]
          |> when_run_with [2;1]
          |> to_complete
          |> and_output [-1]
        );
      );
    ;;
  end

end

let () =
  Machine.Tests.run ();
;;
