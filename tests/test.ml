let rec pp_edge fmt edge =
  Fmt.pf fmt "%a" Fmt.(Dump.pair int pp_node) edge 
and pp_node fmt node =
  Fmt.pf fmt "{ id = %d; transitions = %a; epsilon = %a }" 
    Regex.(node.id)
    Fmt.(Dump.list pp_edge) Regex.(node.transitions)
    Fmt.(Dump.list pp_node) Regex.(node.epsilon) 

let pp_nfa fmt nfa =
  Fmt.pf fmt "%a" Fmt.(Dump.pair pp_node pp_node) nfa 

let node_checker = Alcotest.testable pp_node (=) 

let nfa_check = Alcotest.testable pp_nfa (=)

let pp_matrix fmt matrix =
  Fmt.pf fmt "%a" Fmt.(Dump.array (Dump.array (Dump.pair int int))) matrix

let matrix_check = Alcotest.testable pp_matrix (=)

let final = Regex.({ id = 1; transitions = []; epsilon = [] })

let char_test () =
  let nfa = Regex.compile (Regex.char_regex 'a') in
  let open Regex in
  let expected = 
    ({ id = 2;
       transitions = [97, final];
       epsilon = []}, final) in 
  Alcotest.(check nfa_check) "nfa char regex correct" expected nfa

let epsilon_test () =
  let nfa = Regex.compile Regex.epsilon in
  let final = { final with id = 3 } in 
  Alcotest.(check nfa_check) "nfa epsilon regex correct" (final, final) nfa

let concat_test () =
  let a = Regex.char_regex 'a' in
  let b = Regex.char_regex 'b' in
  let nfa = Regex.compile (Regex.concat a b) in
  let open Regex in
  let final = { id = 4; transitions = []; epsilon = [] } in
  let expected =
    ({ id = 6;
       transitions = [
         (97, { id = 5; transitions = [(98, final)]; epsilon = [] })];
       epsilon = [] },
     final) 
  in
  Alcotest.(check nfa_check) "concat regex compilation" expected nfa

let alternate_test () =
  let a = Regex.char_regex 'a' in
  let b = Regex.char_regex 'b' in
  let nfa = Regex.compile (Regex.alternate a b) in
  let open Regex in
  let final = { id = 7; transitions = []; epsilon = [] } in
  let expected =
    ({ id = 8;
       transitions = [];
       epsilon = [
         {id = 10; transitions = [(97, final)]; epsilon = []};
         {id = 9; transitions = [(98, final)]; epsilon = []};
       ] },
     final)
  in
  Alcotest.(check nfa_check) "alternate regex compilation" expected nfa

let optional_test () =
  let a = Regex.char_regex 'a' in
  let nfa = Regex.compile (Regex.optional a) in
  let open Regex in
  let final = { id = 11; transitions = []; epsilon = [] } in
  let expected =
    ({ id = 12;
       transitions = [];
       epsilon = [
         {id = 13; transitions = [(97, final)]; epsilon = []};
         final
       ]
     }, final) in
  Alcotest.(check nfa_check) "optional regex compilation" expected nfa

let star_test () =
  let a = Regex.char_regex 'a' in
  let nfa = Regex.compile (Regex.star a) in
  let open Regex in
  let final = { id = 14; transitions = []; epsilon = [] } in
  let expected =
    ({ id = 15;
       transitions = [];
       epsilon = [
         { id = 16; transitions = [97, final]; epsilon = [] };
         final
       ]},
    final)
  in
  Alcotest.(check nfa_check) "star regex compilation" expected nfa

(* can't enable this test, because of mutually recursive nodes constructed by a plus-regex. *)
let plus_test () =
  let a = Regex.char_regex 'a' in
  let nfa = Regex.compile (Regex.plus a) in
  let open Regex in
  Alcotest.(check nfa_check) "plus regex compilation" nfa nfa
  

let transition_matrix_test () =
  ()
  (* let nfa = Regex.compile (Regex.concat (Regex.char_regex 'a') (Regex.char_regex 'b')) in *)
  (* let expected = [|[|(97, 1)|]; [|(98, 2)|]; [||]|] in *)
  (* Alcotest.(check matrix_check) "builds transition_matrix" *)
    (* (Regex.construct_dfa_transition_matrix nfa) *)
    (* expected *)

let nfa_test_set = [
  "char regex" , `Quick, char_test;
  "epsilon regex", `Quick, epsilon_test;
  "concat regex", `Quick, concat_test;
  "alternate regex", `Quick, alternate_test;
  "optional regex", `Quick, optional_test;
  "star regex", `Quick, star_test;
  (* "plus regex", `Quick, plus_test; *)
]

let determization = [
  "transition matrix", `Quick, transition_matrix_test
]

(* Run it *)
let () =
  Alcotest.run "Regex" [
    "regex -> nfa", nfa_test_set;
    "nfa -> dfa", determization;
  ]


