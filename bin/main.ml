(* bin/main.ml: command-line entry point for llang *)

open Llang.Parser_utils
open Llang.Interpreter
open Llang.Typing

(* ---------- Reading the program ---------- *)

let read_file file =
  In_channel.with_open_text file In_channel.input_all

let read_stdin () =
  In_channel.input_all stdin

(* ---------- Pipelines ---------- *)

let interpret src =
  let ast = parse_string src in
  let _typed = type_synth ast in (* Reject non well-typed expressions *)
  print_endline (string_of_value (eval ast))

let compile src =
  let ast = parse_string src in
  let typed = type_synth ast in
  print_string (Llang.Llvm_utils.string_of_program typed)

(* Debug: print the program after closure conversion. *)
let closure_convert src =
  let typed = type_synth (parse_string src) in
  print_string (Llang.Closure_ir.string_of_program (Llang.Closure_conv.convert_program typed))

(* ---------- Errors ---------- *)

(* Exception -> Message mapping. Every message starts with its kind:
   "Parse error", "Lexing error", "Type error", "Runtime error", ... *)
let message_of_exception = function
  | Failure msg -> msg
  | Llang.Lexer.Lexing_error msg -> "Lexing error: " ^ msg
  | Division_by_zero -> "Runtime error: division by zero"
  | Sys_error msg -> "Error: " ^ msg
  | e -> "Unexpected error: " ^ Printexc.to_string e

(* ---------- REPL ---------- *)

(* Interactive mode: on error, report and keep going. *)
let repl () =
  print_endline "Welcome to the L language REPL & Compiler! (in OCaml)";
  print_endline "Insert an expression. Ctrl+D to exit.";
  let rec loop () =
    print_string "> "; flush stdout;
    match read_line () with
    | line ->
        (try interpret line with e -> prerr_endline (message_of_exception e));
        loop ()
    | exception End_of_file -> print_newline ()
  in
  loop ()

(* ---------- Command line ---------- *)

let usage () =
  prerr_endline "usage: llang -i [file] | llang -c [file] | llang --cc [file]";
  exit 2

(* Batch mode: any error that escapes is reported once, here, and the
   program exits with status 1. ([exit] does not raise an exception, so
   [usage]'s exit 2 is not caught by this handler.) *)
let () =
  try
    match Array.to_list Sys.argv |> List.tl with
    | ["-i"; file] -> interpret (read_file file)
    | ["-i"]       -> repl ()
    | ["-c"; file] -> compile (read_file file)
    | ["-c"]       -> compile (read_stdin ())
    | ["--cc"; file] -> closure_convert (read_file file)
    | ["--cc"]       -> closure_convert (read_stdin ())
    | _            -> usage ()
  with e ->
    prerr_endline (message_of_exception e);
    exit 1
