open Ast 

let parse (S : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

let interp (s : string) : string = 
  failwith "unimplemented"