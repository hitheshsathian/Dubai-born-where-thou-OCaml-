open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)

let extend env x v = (x,v) :: env

let rec lookup (env: environment ) (x: string) =
  match env with
  | [] -> raise UndefinedVar
  | (y,v) :: env' -> if x = y then v else lookup env' x 


let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    | Number x -> Int_Val x
    | True -> Bool_Val true
    | False -> Bool_Val false
    | Var x -> lookup env x
    | Plus (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        |Int_Val r1, Int_Val r2 -> Int_Val (r1+r2)
        | _ -> raise TypeError)
    | Minus (a,b) -> 
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
          | Int_Val r1, Int_Val r2 -> Int_Val (r1-r2)
          | _ -> raise TypeError)
    | Times (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
          | Int_Val r1, Int_Val r2 -> Int_Val (r1*r2)
          |_ -> raise TypeError)
    | Div (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
          | Int_Val r1, Int_Val r2 -> if r2 = 0 then raise DivByZeroError else Int_Val (r1/r2)
          | _ -> raise TypeError)
    | Mod (a,b) -> 
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        | Int_Val r1, Int_Val r2 -> if r2 = 0 then raise DivByZeroError else Int_Val(r1 mod r2)
        | _ -> raise TypeError)
    | Or (a,b) -> 
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        | Bool_Val r1, Bool_Val r2 -> Bool_Val(r1||r2)
        | _ -> raise TypeError)
    | And (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        | Bool_Val r1, Bool_Val r2 -> Bool_Val(r1&&r2)
        | _ -> raise TypeError)
    | Not a ->
        let r1 = eval_expr a env in
        (match r1 with
        |Bool_Val r1 -> Bool_Val (not r1)
        |_ -> raise TypeError)
    | Lt (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        | Int_Val r1, Int_Val r2 -> Bool_Val(r1<r2)
        |_ -> raise TypeError)
    | Leq (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        | Int_Val r1, Int_Val r2 -> Bool_Val(r1<=r2)
        |_ -> raise TypeError)
    | Eq (a,b) ->
        let r1 = eval_expr a env in
        let r2 = eval_expr b env in
        (match r1, r2 with
        | Int_Val r1, Int_Val r2 -> Bool_Val(r1=r2)
        |_ -> raise TypeError)
    | Fun (x,e) -> Closure (env, x, e)
    | App(e1,e2) ->
        let r1 = eval_expr e1 env in
        let r2 = eval_expr e2 env in
        (match r1, r2 with
        | Closure(env, x, e), v -> eval_expr e (extend env x (v))
        |_ -> raise UndefinedVar) 
        
    
(* evaluate a command in an environment *)
let rec assg_helper ((env: environment), x, v) = 
  let r1 = lookup env x in
  let r2 = eval_expr v env in
  (match r1,r2 with
  |Int_Val x1, Int_Val y1 -> extend env x r2
  |Bool_Val x1, Bool_Val y1 -> extend env x r2
  |Closure _, Closure _-> extend env x r2)

let rec while_helper (c : com) (env : environment) = 
  match env with
  |_ -> raise TypeError
  | env -> while_helper c env 

let rec for_helper (n : int) (c : com) (env : environment) = 
  match n with
  |_-> raise TypeError
  | n -> if n>0 then for_helper (n-1) c env else 0 

let rec eval_command (c : com) (env : environment) : environment =
    match c with
    | Declare (v, (x : string)) -> 
      (match v with
      |Bool_Type -> env @ [(x, Bool_Val(false))]
      |Int_Type -> env @ [(x, Int_Val(0))]
      |Lambda_Type -> env @ [(x, Closure(env, "x", Var "x"))])

    | Assg ((x : string), v) -> assg_helper(env, x, v)
    | Skip -> env
    | Comp (c1, c2) -> let env' = eval_command c1 env in eval_command c2 env'
    | Cond (e, c1, c2) ->
      (match e with
      |_ -> raise TypeError
      | e -> if true then eval_command c1 env else eval_command c2 env)
    | While (e, c) -> 
      (match e with
      |_ -> raise TypeError
      | e -> if true then (let env' = eval_command c env in while_helper c env') else eval_command c env)
    | For (e, c) -> []
    
       

