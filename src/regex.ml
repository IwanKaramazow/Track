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
  
