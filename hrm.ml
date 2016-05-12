module Machine = struct

  type 'a result =
    | Completed of 'a list * int
    | Aborted of string
  ;;

  let run ~program ~input ~memory ~max_steps =
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

    let rec loop steps pc register input output =
      let step = loop (steps + 1) in
      let next = pc + 1 in
      let no_more_input = (List.length input) = 0 in
      let at_the_end = (pc = the_end) in
      let read_reg fn =
        match register with
        | None -> Aborted("Empty register")
        | Some data -> fn data
      in

      if steps = max_steps then
        Aborted(Printf.sprintf "Maximum number of steps (%d) reached" max_steps)
      else if at_the_end then
        if no_more_input then Completed(List.rev output, steps)
        else Aborted(Printf.sprintf "Not all input is processed. Items left: %d" (List.length input))
      else
        begin match program.(pc) with
        | `Input ->
          begin match input with
          | [] -> Completed(List.rev output, steps + 1)
          | data :: input -> step next (Some(data)) input output
          end
        | `Output ->
          begin match register with
          | None -> Aborted("Output: Empty output not allowed")
          | Some data -> step next None input (data :: output)
          end
        | `Jump addr ->
          in_prog_range addr (fun addr -> step addr register input output)
        | `JumpIfZero addr ->
          in_prog_range addr (fun addr ->
            read_reg (fun data -> step (if data = 0 then addr else next) register input output)
          )
        | `JumpIfNegative addr ->
          in_prog_range addr (fun addr ->
            read_reg (fun data -> step (if data < 0 then addr else next) register input output)
          )
        | `CopyTo addr ->
          in_mem_range addr (write_mem register (fun () -> step next register input output))
        | `CopyFrom addr ->
          in_mem_range addr (read_mem (fun data -> step next (Some(data)) input output))
        | `Add addr ->
          in_mem_range addr (read_mem (fun data ->
            read_reg (fun reg -> step next (Some(reg + data)) input output))
          )
        | `Sub addr ->
          in_mem_range addr (read_mem (fun data ->
            read_reg (fun reg -> step next (Some(reg - data)) input output))
          )
        end
    in
    loop 0 0 None input []
  ;;

  module Tests = struct
    exception Failure of string;;
    type ('a, 'b) computer = { program: 'a array; memory: 'b array };;

    let describe text fn =
      Printf.printf "\n--- %s ---\n" text;
      fn ();
    ;;

    let case text fn =
      let green = Printf.sprintf "\027[32m%s\027[0m" in
      let red = Printf.sprintf "\027[31m%s\027[0m" in
      try
        fn ();
        Printf.printf "[%s] %s\n" (green "pass") text
      with
      | Failure reason -> Printf.printf "[%s] %s :: %s\n" (red "fail") text reason
      | e -> Printf.printf "[%s] %s :: %s\n" (red "fail") text (Printexc.to_string e)
    ;;

    let expect program =
      { program = program; memory = [||] }
    ;;

    let with_memory mem comp =
      { comp with memory = mem }
    ;;

    let when_run_with input comp =
      run ~program:comp.program ~input:input ~memory:comp.memory ~max_steps:256
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

        case "abort when max steps reached" (fun () ->
          expect [|`Jump(0)|] |> when_run_with [] |> to_abort
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
