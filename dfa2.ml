(* Experimentation with Deterministic Finite Automata (DFA) *)

type node = {
  id: int;
  transitions: edge list
} and edge = (int * node) 
  

(* a DFA has a start & final node *)
type dfa = (node * node)

(* Don't hard code the transition matrix *)
(* let transition_matrix =  *)
  (* [|              (* a; b *) *)
    (* (* state 0 *) [| 0; 0 |] *)
  (* ; (* state 1 *) [| 2; 1 |] *)
  (* ; (* state 2 *) [| 2; 1 |] *)
  (* |] *)

(* 97 -> 'a'; 98 -> 'b' *)
let rec node1 = {
  id = 1;
  transitions = [(97, node2); (98, node1)]
} and node2 = {
  id = 2;
  transitions = [(97, node2); (98, node1)]
}

let dfa = (node1, node2)

let is_final_state dfa node = 
  let (_, final_node) = dfa in
  node.id == final_node.id


let transition node c =
  let char_code = Char.code c in
  let (_, next_node) = List.find (fun (code, _) -> code == char_code) node.transitions in
  next_node

let () = 
  let parse dfa s = 
    let input = s in
    let n = String.length input in
    let (start_node, _) = dfa in
    let rec walk pos node =
      if pos == n then
        node
      else
        let next_node = transition node (String.get input pos) in
        walk (pos + 1) next_node
    in
    let final_node = walk 0 start_node in
    match (is_final_state dfa final_node) with
    | true -> print_endline "correct string according to dfa!"
    | false -> print_endline  "string didn't match dfa"
  in
  let rec main () =
    let input = read_line () in
    parse dfa input;
    main ()
  in
  main ()
