module Machine = struct

  type result =
    | Done of int
    | Error of string
  ;;

  let null_writer = fun _ -> ();;

  let run ~program ~inputs ~outputs ~memory_size ~log_writer =
    let the_end = Array.length program in
    let in_prog_range addr fn =
      if addr < 0 || addr >= the_end
      then Error(Printf.sprintf "Program address out of range: %d" addr)
      else fn addr
    in
    let memory = Array.init memory_size (fun _ -> None) in
    let in_mem_range addr fn =
      if addr < 0 || addr >= memory_size
      then Error(Printf.sprintf "Memory address out of range: %d" addr)
      else fn addr
    in
    let read_mem fn addr =
      match memory.(addr) with
      | None -> Error(Printf.sprintf "Memory empty at address %d" addr)
      | Some data -> fn data
    in
    let write_mem data fn addr =
      match data with
      | None -> Error("Register is empty")
      | Some _ -> memory.(addr) <- data; fn ()
    in

    let rec loop steps pc register inputs outputs =
      let no_more_input = (List.length inputs) = 0 in
      let no_more_output = (List.length outputs) = 0 in
      let at_the_end = (pc = the_end) in
      let log msg = log_writer (Printf.sprintf "[%d:%d] %s" steps pc msg) in
      let read_reg fn =
        match register with
        | None -> Error("Empty register")
        | Some data -> fn data
      in

      if at_the_end then
        if no_more_input then
          if no_more_output then Done(steps)
          else Error(Printf.sprintf "Missing output. Items left: %d" (List.length outputs))
        else Error(Printf.sprintf "Not all input is processed. Items left: %d" (List.length inputs))
      else
        begin match program.(pc) with
        | `Input ->
          log "Input";
          begin match inputs with
          | [] ->
            if no_more_output then Done(steps + 1)
            else Error(Printf.sprintf "Input: All input processed, missing output. Items left: %d" (List.length outputs))
          | data :: inputs_left -> loop (steps + 1) (pc + 1) (Some(data)) inputs_left outputs
          end
        | `Output ->
          log "Output";
          begin match outputs with
          | [] -> Error("Output: No more output expected")
          | expected :: outputs_left ->
            begin match register with
            | None -> Error("Output: Empty output not allowed")
            | Some actual ->
              if actual != expected then Error(Printf.sprintf "Output: expected %d, got %d" expected actual)
              else loop (steps + 1) (pc + 1) None inputs outputs_left
            end
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
    loop 0 0 None inputs outputs
  ;;

  module Tests = struct
    let pass fmt = Printf.printf (Printf.sprintf "pass: %s\n" fmt);;
    let fail fmt = Printf.printf (Printf.sprintf "fail: %s\n" fmt);;

    let expect program inputs outputs ?memory_size:(memory_size=0) m =
      fun () -> (m, (run ~program ~inputs ~outputs ~memory_size ~log_writer:null_writer))
    ;;

    let to_be_done_in expected fn =
      let m, res = fn () in
      match res with
      | Error reason -> fail "%s (%s)" m reason
      | Done actual ->
        if actual != expected then
          Printf.printf "fail: %s - expected %d, got %d steps\n" m expected actual
        else
          pass "%s" m
    ;;

    let to_be_done fn =
      let m, res = fn () in
      match res with
      | Error reason -> Printf.printf "fail: %s (%s)\n" m reason
      | Done _ -> Printf.printf "pass: %s\n" m
    ;;

    let to_error fn =
      let m, res = fn () in
      match res with
      | Error reason -> Printf.printf "pass: %s\n" m
      | Done _ -> Printf.printf "fail: %s - expected an error" m
    ;;

    let describe text fn =
      Printf.printf "\n--- %s ---\n" text;
      fn ();
    ;;


    let run () =
      describe "simple" (fun () ->
        "in/out" |> expect [|`Input;`Output|] [1] [1] |> to_be_done_in 2;
        "loop" |> expect [|`Input;`Output;`Jump(0)|] [1;2] [1;2] |> to_be_done_in 7;
      );

      describe "program end" (fun () ->
        "missing output" |> expect [|`Output|] [1] [1] |> to_error;
        "last input read" |> expect [|`Input;`Input;`Output|] [1] [] |> to_be_done_in 2;
      );

      describe "branching" (fun () ->
        "jump if zero" |> expect [|`Input;`JumpIfZero(0);`Output;`Jump(0)|] [0;1;0;2;0] [1;2] |> to_be_done;
        "jump if negative" |> expect [|`Input;`JumpIfNegative(0);`Output;`Jump(0)|] [0;1;-1;2;-2] [0;1;2] |> to_be_done;
      );

      describe "intermediate" (fun () ->
        "memory" |> expect ~memory_size:1 [|`Input;`CopyTo(0);`CopyFrom(0);`Output|] [1] [1] |> to_be_done;
        "adding" |> expect ~memory_size:1 [|`Input;`CopyTo(0);`Add(0);`Output|] [1] [2] |> to_be_done;
        "subtracting" |> expect ~memory_size:1 [|`Input;`CopyTo(0);`Input;`Sub(0);`Output|] [2;1] [-1] |> to_be_done;
      );
    ;;
  end

end

let () =
  Machine.Tests.run ();
;;
