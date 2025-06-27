
#use "topfind"

#require "z3"
#require "str"

open Z3

let fname = ref ""
let ctx = Z3.mk_context []

let smt fname =
  SMT.parse_smtlib2_file ctx fname [] [] [] [] |> AST.ASTVector.to_expr_list

let solver = Solver.mk_simple_solver ctx

let model fname =
  Solver.check solver (smt fname) |> function
  | Solver.UNSATISFIABLE ->
      print_endline "unsat";
      exit 0
  | Solver.UNKNOWN ->
      print_endline "unknown";
      exit 1
  | Solver.SATISFIABLE -> (
      print_endline "sat";
      Solver.get_model solver |> function
      | None -> failwith "Unable to get model"
      | Some x -> x)

type name_tok = { pref : string; name : string; tok : int option }

let pref = Str.regexp {|\(source\|target\)__\(.*\)|}
let nameP = Str.regexp {|\(source\|target\)__\(.*\)_AT\([0-9]+\)|}

let tokenise_name n =
  if Str.string_match nameP n 0 then
    let pref = Str.matched_group 1 n in
    let name = Str.matched_group 2 n in
    let tok =
      try Str.matched_group 3 n |> int_of_string |> fun c -> Some c
      with Not_found -> None
    in
    { pref; name; tok }
  else if Str.string_match pref n 0 then
    let pref = Str.matched_group 1 n in
    let name = Str.matched_group 2 n in
    { pref; name; tok = None }
  else { pref = ""; name = n; tok = None }

module NT = struct
  type t = name_tok

  let compare a b =
    String.compare a.pref b.pref |> function
    | 0 -> (
        String.compare a.name b.name |> function
        | 0 -> (
            match (a.tok, b.tok) with
            | None, None -> 0
            | Some a, Some b -> Int.compare a b
            | Some a, None -> 1
            | None, Some b -> -1)
        | n -> n)
    | n -> n

  let to_string n =
    let tok =
      match n.tok with None -> "" | Some n -> Printf.sprintf "_AT%d" n
    in
    Printf.sprintf "%s%s%s" n.pref n.name tok
end

module M = Map.Make (NT)
module StringMap = Map.Make (String)

let consts m =
  Model.get_const_decls m
  |> List.filter_map (fun f ->
         let i = Model.get_const_interp m f in
         Option.bind i (fun interp ->
             if Expr.get_num_args interp = 0 then
               let n =
                 FuncDecl.get_name f |> Symbol.to_string |> tokenise_name
               in
               Some (n, interp)
             else None))

let consts model : (name_tok * Expr.expr) list = consts model

let source consts =
  consts
  |> List.filter_map (function
       | { pref = "source"; name; tok }, e -> Some ({ pref = ""; name; tok }, e)
       | t, e -> None)
  |> M.of_list

let target consts =
  consts
  |> List.filter_map (function
       | { pref = "target"; name; tok }, e -> Some ({ pref = ""; name; tok }, e)
       | _ -> None)
  |> M.of_list

let differences source target =
  print_endline "Equal:";
  M.merge
    (fun i a b ->
      match (a, b) with
      | Some a, Some b when a = b ->
          Printf.printf "(matches) %s = %s\n" (NT.to_string i)
            (Expr.to_string b);
          None
      | Some a, None ->
          Printf.printf "not defined in target: %s %s\n" (NT.to_string i)
            (Expr.to_string a);
          None
      | None, Some b ->
          Printf.printf "not defined in source: %s %s\n" (NT.to_string i)
            (Expr.to_string b);
          None
      | Some a, Some b ->
          (*Printf.printf "(source, target) differ in %s by:\n  %s != %s\n" 
            (NT.to_string i) (Expr.to_string a) (Expr.to_string b); *)
          Some (a, b)
      | None, None -> None)
    source target
  |> M.to_list
  |> List.sort (fun (a, _) (b, _) -> NT.compare a b)
  |> fun e ->
  print_endline "";
  print_endline "differing:";
  List.iter
    (fun (ident, (e1, e2)) ->
      Printf.printf "%s\n  %s != %s\n" (NT.to_string ident) (Expr.to_string e1)
        (Expr.to_string e2))
    e

let print_consts m =
  consts m
  |> List.iter (fun (name, interp) ->
         Printf.printf "%s %s = %s\n" name.pref name.name
           (Expr.to_string interp))

let filename = ref ""
let usage_msg = "bastvtracer -i <file>"
let spec = [ ("-i", Arg.Set_string filename, "Set input file name") ]

let () =
  Arg.parse_argv Sys.argv spec ignore usage_msg;
  if !filename = "" then (
    print_endline "missing -i";
    Arg.usage spec usage_msg;
    exit 1);
  let m = model !filename in
  let cst = consts m in
  let src = source cst in
  let tgt = target cst in
  differences src tgt;
  ()
