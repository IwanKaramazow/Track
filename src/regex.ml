(* type regex = *)
  (* | Alternation of (char * char) *)

type node = {
  id: int;
  mutable transitions: edge list;
  mutable epsilon: node list;
}
and edge = (int * node)

let id = ref 0

let gen_id () =
  id := !id + 1;
  !id

let new_node () =
  { id = gen_id ();
    transitions = [];
    epsilon = [] }

let char_regex c next =
  let node = new_node () in
  node.transitions <- node.transitions @ [Char.code c, next];
  node 

let transition (dfa_state : node list)  c =
  let char_code = Char.code c in
  (* Just crash if we can't make a transition to the next node, for now... *)
  (* let (_, next) =  List.find (fun (code, _) -> code == char_code) node.transitions *)
  (* in next *)
  let nodes = List.filter (fun {transitions} -> 
    let filtered = List.filter ( fun (code, _) -> code == char_code) transitions in
    List.length filtered > 0) dfa_state in
  List.flatten @@ List.map (fun node -> List.map snd node.transitions) nodes 

(* nfa -> dfa *)
(* a single state in a dfa is a set of multiple nfa states *)
(* epsilon-closures*)
let closure node =
  node::node.epsilon
 
let concat re1 re2 next =
  (* wat moet er hier gebeuren? 
   * de edge uit nfa1 moet naar nfa2 verwijzen
   * transitions van nfa1 updaten naar nfa2 ?? 
   * betekent dit dat het einde van nfa1 & de start van nfa2 gemerged moeten worden ?
   * 
   * eigenlijk is dit anders bekeken, gewoon:
   * (M . N)(node) = M(N(node)) 
   * Regex is gewoon een functie over toestand van een automaton ? *)
   re1 (re2 next)

(* alternation *)
let alternate re1 re2 next =
  let node = new_node () in
  node.epsilon <- [re1 next; re2 next];
  node

(* can be simplified by just returning next *)
let epsilon next =
  next
    
let star regex next =
  let node = new_node () in
  node.epsilon <- [regex next; next];
  node

let plus regex next = 
  (* (* concat regex (star regex) *) *)
  let node = new_node () in
  let nr = regex node in
  node.epsilon <- [nr; next];
  nr

let optional re =
  alternate re  epsilon

(* regex -> nfa 
 * transforms a regex into its nfa form
 * also stores which state of the nfa is final *)
let compile regex =
  let final_node = new_node () in
  (regex final_node, final_node)

(* [0 - 255] *) 
let alphabet_size = 256

(* dog slow *)
let find_index needle haystack =
  let rec find i needle = function
    | [] -> raise Not_found
    | x::xs -> if x == needle then i else find (i + 1) needle xs 
  in
  find 0 needle haystack

let rec add_node state node =
  if List.memq node state then state else add_nodes (node::state) node.epsilon
and add_nodes state nodes =
  List.fold_left add_node state nodes

(* TODO: epsilon closure *)
let dfa_edge nfa = 
  Array.of_list nfa.transitions
  (* let ts = List.map (fun (charCode, node) -> (charCode, add_nodes [] node)) nfa.transitions in *)
  (* Array.of_list ts *)

let create_alphabet_arr () = Array.make 256 0

let construct_dfa_transition_matrix nfa =
  let (start, _final) = nfa in
  let states = Hashtbl.create 20 in
  let transitions = Hashtbl.create 20 in

  let counter = ref 0 in

  let rec aux nfa =
    try Hashtbl.find states nfa
    with Not_found ->
      let i = !counter in
      incr counter;
      let e = dfa_edge nfa in
      let e2 = Array.map (fun (charCode, node) -> charCode, aux node) e in
      Hashtbl.add transitions i e2;
      i
  in
  let count_ = aux start in
  (* assert (count = 0); *)
  Array.init !counter (Hashtbl.find transitions)
  |> Array.map (fun item ->
    let alphabet_arr = create_alphabet_arr () in
    Array.iter (fun (charCode, nextState) -> 
      Array.set alphabet_arr charCode nextState;
      ) item;
    alphabet_arr)


type dfa_state = node list

let match_ word transition_matrix =
  let len = String.length word in
  let rec aux i state =
    if i == len then
      begin match state with
      | 0 -> true
      | _ -> false
      end
    else
      begin
    let arr = transition_matrix.(state) in
    let charCode = Char.code (String.get word i) in
    print_endline (string_of_int i);
    print_endline (string_of_int charCode);
    begin match arr.(charCode) with
    | 0 -> if i == (len - 1) then true else false
    | nextState -> aux (i + 1) nextState
    end
      end
  in
  aux 0 0



(* let rec add_node state node = *)
  (* if List.memq node state then state else add_nodes (node::state) node.epsilon *)
(* and add_nodes state nodes = *)
  (* List.fold_left add_node state nodes *)

(* let () = *)
  (* let rsa = compile (char_regex 'a') in *)
  (* let rsb = compile (char_regex 'b') in *)
  (* let rsc = compile (char_regex 'a') in *)
  (* let init = ref [] in  *)
  (* Array.iter (fun (i,_) -> init :=  add_node !init i) [| rsa; rsb; rsa |]; *)
  (* let _ = 1 in *)
  (* () *)

(* let () = *)
  (* let regex = char_regex 'a' in *)
  (* let regex2 = char_regex 'b' in *)
  (* let compiled = alternate (char_regex 'a') (concat regex regex2) in *)
  (* let compiled = star regex in *)

  (* let rec walk pos nfa_nodes input = *)
    (* if (pos == String.length input) then *)
      (* nfa_nodes *)
    (* else *)
      (* let dfa_state = List.map closure nfa_nodes |> List.flatten in *)
      (* let next = transition dfa_state (String.get input pos) in *)
      (* walk (pos + 1) next input *)
  (* in *)
  
  (* let rec repl () = *)
    (* let input = read_line () in *)
    (* let () =  *)
      (* let start = compiled (new_node ()) in *)
      (* begin match walk 0 [start] input with *)
      (* | [] -> print_endline "string is unrecognized" *)
      (* | _ -> print_endline "your string is in our language" *)
      (* | _ -> print_endline "your string is in our language" *)
      (* | exception _ -> print_endline "string is unrecognized" *)
      (* end *)
    (* in *)
    (* repl () *)
  (* in *)
  (* repl () *)
  
