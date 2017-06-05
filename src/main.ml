open Format
open Expr
open Printing

exception Error of string
let error msg = raise (Error msg)

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> error "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> error "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> error ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexing.from_channel pi
  in let result =
    try Parser.expr_semi_seperated Lexer.token lexbuf with Parsing.Parse_error -> 
    error "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let process_file f =
  alreadyImported := f :: !alreadyImported;
  let expr_list = parseFile f in
  let rec construct_rec_slet = (fun inner_expr -> function
    | (name, expr)::rest -> SLet(name, expr, construct_rec_slet inner_expr rest)
    | [] -> inner_expr
  ) in
  ignore (List.fold_left (fun (bound_tuple_list_rev, i) (bind_name, expr) ->
    let default = (bound_tuple_list_rev, i + 1) in
    try
      let constructed_expr = construct_rec_slet expr (List.rev bound_tuple_list_rev) in
      let t_expr = Infer.infer_expr Core.plain_env 0 constructed_expr in
      Refined.check_expr t_expr;
      let eval_result = Eval.eval Eval.Ctx.StringMap.empty constructed_expr in
      (match bind_name with
        | Some name ->
          print_endline (name ^ ": " ^ (string_of_t_ty (Infer.generalize (-1) t_expr.ty)));
          ((name, eval_result)::bound_tuple_list_rev, i + 1)
        | None -> 
          (try Draw.draw eval_result with Draw.Error ->
          print_endline ((string_of_s_expr eval_result) ^ ": " ^ (string_of_t_ty (Infer.generalize (-1) t_expr.ty))));
          default)
    with 
      | Infer.Error msg ->
        print_endline ("Infer error in statement " ^ (string_of_int i) ^ ": " ^ msg); default
      | Refined.Error msg ->
        print_endline ("Refined error in statement " ^ (string_of_int i) ^ ": " ^ msg); default
      | Eval.Error msg ->
        print_endline ("Eval error in statement " ^ (string_of_int i) ^ ": " ^ msg); default
  ) ([], 1) expr_list);
  read_line ()

let main () = 
  Draw.init ();
  let inFile = parseArgs () in
  process_file inFile;;

main ();