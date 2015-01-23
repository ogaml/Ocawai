open OUnit2
open Lexing
open Checker

(* Testing environment *)
let test file f =
  ScriptCore.init () ;
  let input = open_in file in
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file} ;
  try
    let script = Parser.file Lexer.token lexbuf in
    close_in input;
    type_check script [];
    let script = Interpreter.new_script script [] in
    f script
  with
  | Lexer.Script_SyntaxError _ ->
      assert_failure "lexing error"
  | Parser.Error ->
      assert_failure "parsing error"
  | Type_checking_failure ->
      assert_failure "type checking error"

(* check assoc *)
let test_assoc test_ctxt =
  test "test/script_test_assoc.script" (fun script ->
    assert_equal (Interpreter.call_f script "test_assoc") (`Bool true)
  )

(* Test the typing of values *)
let value_types _ =
  test "test/typechecker.script" (fun s ->
    assert_equal true true
  )

let suite_script =
  test_list [
    "script test" >::: [
      "check assoc" >:: test_assoc
    ] ;
    "type checker" >::: [
      "value types" >:: value_types
    ]
  ]

let () =
  run_test_tt_main suite_script
