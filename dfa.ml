(* Experimentation with Deterministic Finite Automata (DFA) *)

let transition_matrix = 
  [|              (* a; b *)
    (* state 0 *) [| 0; 0 |]
  ; (* state 1 *) [| 2; 1 |]
  ; (* state 2 *) [| 2; 1 |]
  |]

let is_final_state = function
  | 2 -> true
  | _ -> false

let matrix_index_of_int = function
  | 97 -> 0 
  | 98 -> 1
  | _ -> assert false


let transition m state c =
  let transitions = m.(state) in
  let i = matrix_index_of_int (Char.code c) in
  let next_state = transitions.(i) in
  next_state

let () = 
  let parse s = 
    let input = s in
    let n = String.length input in
    let rec dfa pos state =
      if pos == n then
        state
      else
        let next_state = transition transition_matrix state input.[pos] in
        dfa (pos + 1) next_state
    in
    let final_state = dfa 0 1 in
    match (is_final_state final_state) with
    | true -> print_endline "correct string according to dfa!"
    | false -> print_endline  "string didn't match dfa"
  in
  let rec main () =
    let input = read_line () in
    parse input;
    main ()
  in
  main ()




