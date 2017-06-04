open Format
open Eval

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
    try Parser.expr_eof Lexer.token lexbuf with Parsing.Parse_error -> 
    error "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let process_file f ctx =
  alreadyImported := f :: !alreadyImported;
  let expr = parseFile f in
  try
    let t_expr = Infer.infer_expr Core.plain_env 0 expr in
    Refined.check_expr t_expr ;
    Draw.draw (Eval.eval ctx expr) ;
  with Refined.Error msg ->
    error msg

let main () = 
  let inFile = parseArgs() in
  process_file inFile Ctx.StringMap.empty;;

main ();