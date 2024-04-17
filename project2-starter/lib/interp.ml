(* COMP 360H Project 1:  an interpreter for an imperative language.
 *
 * Christian Diaz Herrera
 * Cristina Gonzalez
 * Nishant Aggarwal
 *)

 module E = Ast.Expression
 module S = Ast.Stm
 
 (* 'a IdentMap.t:  the type of maps from identifiers to 'a.
  *)
 module IdentMap = Map.Make(Ast.Id)
 
 (* MultipleDeclaration x is raised when x is declared more than once in a
  * block.
  *)
 exception MultipleDeclaration of Ast.Id.t
 
 (* UnboundVariable x is raised when x is used but not declared.
  *)
 exception UnboundVariable of Ast.Id.t
 
 (* UndefinedFunction f is raised when f is called but has not been defined.
  *)
 exception UndefinedFunction of Ast.Id.t
 
 (* TypeError s is raised when an operator or function is applied to operands
  * of the incorrect type.  s is any (hopefuly useful) message.
  *)
 exception TypeError of string

 exception SecurityError

 module SecurityLabel = struct
    type t =
        | High
        | Low

    let to_string (v : t) : string =
        match v with
        | High -> "High Security"
        | Low -> "Low Security"

    let compare (v : t) (v' : t) : t =
        match (v, v') with
        | (Low, Low) -> Low
        | _ -> High
end

module Primval = struct
   type t =
     | V_Undefined
     | V_None
     | V_Int of int
     | V_Bool of bool
     | V_Str of string
     [@@deriving show]

   (* to_string v = a string representation of v (more human-readable than
    * `show`.
    *)
   let to_string (v : t) : string =
     match v with
     | V_Undefined -> "?"
     | V_None -> "None"
     | V_Int n -> Int.to_string n
     | V_Bool b -> Bool.to_string b
     | V_Str s -> s
 end

 (* Values.
  *)
 module Value = struct
    type t = Val of Primval.t * SecurityLabel.t
 
   (* to_string v = a string representation of v (more human-readable than
    * `show`.
    *)
   let to_string (v : t) : string =
     match v with
     | Val (x, y) -> Primval.to_string (x) ^ SecurityLabel.to_string (y)

 end
 
 (* An implementation of the I/O API.  This is a little bit complex, because
  * this one implementation allows for a few variations:
  * - The input and output channel can be set by the client (default to
  *   standard input and output).
  * - The display of prompts (for the prompt_* functions) can be turned off
  *   (default on).
  * These variations let us use this module for interactive use (use the
  * defaults) and testing (redirect the i/o channels to a programmatic stream
  * and turn off the display of prompts.
  *
  * A client makes changes to the defaults by setting `in_channel`,
  * `out_channel`, and `show_prompts`.
  *)
  module Api = struct
 
   (* Raised when a function is invoked that is not in the API.
    *)
   exception ApiError of string
 
   (* in_channel:  input channel (for get_*, prompt_* ).
    *)
   let in_channel : Scanf.Scanning.in_channel ref = 
     ref Scanf.Scanning.stdin
 
   (* out_channel:  output channel (for print_*, prompt_* when prompts are
    * displayed).
    *)
   let out_channel : Out_channel.t ref = ref Out_channel.stdout
 
   (* show_prompts:  true to display prompts, false to not display.
    *)
   let show_prompts : bool ref = ref true
 
   (* output oc s:  output `s` to `oc` and flush `oc`.
    *)
   let output (oc : Out_channel.t) (s : string) : unit =
     Out_channel.output_string oc s ;
     Out_channel.flush oc
 
   (* outputnl oc s = output `s ^ '\n'` to `oc` and flush `oc`.
    *)
   let outputnl (oc : Out_channel.t) (s : string) : unit =
     output oc (s ^ "\n")
 
   (* The API definition.  The API is specified by a
    * (string*(Value.t->Value.t)) list.  Each element names an API function
    * and provides the code to be executed when the function is called.
    *)
   let api : (Value.t list -> Value.t) IdentMap.t =
     [
       ("print_bool", fun vs ->
         match vs with
         | [Value.V_Bool n] -> 
           outputnl (!out_channel) (Bool.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_bool"
       )
     ; ("get_bool", fun vs ->
         match vs with
         | [] -> Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for get_bool"
       )
     ; ("prompt_bool", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for prompt_bool"
       )
     ; ("print_int", fun vs ->
         match vs with
         | [Value.V_Int n] -> 
           outputnl (!out_channel) (Int.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_int"
       )
     ; ("get_int", fun vs ->
         match vs with
         | [] -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for get_int"
       )
     ; ("prompt_int", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for prompt_int"
       )
     ; ("print_str", fun vs ->
          match vs with
          | [Value.V_Str s] -> 
            outputnl (!out_channel) s ; Value.V_None
          | _ -> raise @@ TypeError "Bad argument type for print_s"
       )
     ; ("get_str", fun vs ->
         match vs with
         | [] -> Value.V_Str (Scanf.bscanf !in_channel "%s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for get_str"
       )
     ; ("prompt_str", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for prompt_str"
       )
     ; ("print_bool_s", fun vs ->
         match vs with
         | [Value.V_Bool n] -> 
           outputnl (!out_channel) (Bool.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_bool_s"
       )
     ; ("get_bool_s", fun vs ->
         match vs with
         | [] -> Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for get_bool_s"
       )
     ; ("prompt_bool_s", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for prompt_bool_s"
       )
     ; ("print_int_s", fun vs ->
         match vs with
         | [Value.V_Int n] -> 
           outputnl (!out_channel) (Int.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_int_s"
       )
     ; ("get_int_s", fun vs ->
         match vs with
         | [] -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for get_int_s"
       )
     ; ("prompt_int_s", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for prompt_int_s"
       )
     ; ("print_str_s", fun vs ->
          match vs with
          | [Value.V_Str s] -> 
            outputnl (!out_channel) s ; Value.V_None
          | _ -> raise @@ TypeError "Bad argument type for print_str_s"
       )
     ; ("get_str_s", fun vs ->
         match vs with
         | [] -> Value.V_Str (Scanf.bscanf !in_channel "%s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for get_str_s"
       )
     ; ("prompt_str_s", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for prompt_str_s"
       )
     ] |> List.to_seq |> IdentMap.of_seq
 
   (* do_call f vs invokes the API function corresponding to `f` with argument
    * list `vs`.
    *
    * Raises ApiError f: if f is not an API function.
    *)
   let do_call (f : string) (vs : Value.t list) : Value.t =
     try
       IdentMap.find f api vs
     with
     | Not_found -> raise @@ ApiError f
 
 
 end
 
 (* The code for Env/Frame Stuffs
 *)
 module IdMap = Map.Make(Ast.Id)
 
     (* Environments.
      *
      * A value of type t is a map from identifiers to values.  We use σ to range
      * over environments and standard function notation when describing them.
      *)
     module Env = struct
 
       type bindingTable = Value.t IdMap.t
 
       type t =
       | FunctionFrame of bindingTable list
       | ReturnFrame of Value.t
 
       (* The type of environments.
        *)
 
       (*HELPER: looks through list of environments to find given identifier. Else raises unbound variable error*)
       let rec lookup' (currFrame : bindingTable list) (x : Ast.Id.t) : Value.t =
         match currFrame with
           | [] -> raise (UnboundVariable x)
           | y :: ys ->
               try
                   IdMap.find x y
               with
                   | Not_found -> lookup' ys x
 
       (*FUNCTION: matches given identifier to identifier in Environment frame and returns value associated*)
       let lookup (currFrame : t) (x : Ast.Id.t) : Value.t =
             match currFrame with
             | FunctionFrame currFrame' -> lookup' currFrame' x
             | ReturnFrame _ -> failwith @@ "Lookup in ReturnFrame"
 
       (*FUNCTION: determines whether a given identifier is found within an environment and which environment, else looks through the rest*)
       let rec varBounded (currFrame : bindingTable list) (x : Ast.Id.t) : bool * bindingTable =
         match currFrame with
         | [] -> raise (UnboundVariable x)
         | y :: ys -> match IdMap.mem x y with
                      | true -> (true, y)
                      | false -> varBounded ys x
         
       (*HELPER: finds identifier in Environment and binds value to identifier*)
       let rec update' (tables : bindingTable list) (x : Ast.Id.t) (v : Value.t): bindingTable list =
         match tables with
         | [] -> raise (UnboundVariable x)
         | currMap :: rest ->
             if IdMap.mem x currMap then
                 (IdMap.add x v currMap) :: rest
             else
                 let updatedRest = update' rest x v in
                     currMap :: updatedRest
 
       (*FUNCTION: given an identifier and value, binds identifier to value*)
       let update (currFrame : t) (x : Ast.Id.t) (v : Value.t) : t =
         match currFrame with
             | ReturnFrame _ -> failwith "Update in a return Frame"
             | FunctionFrame currFrame -> FunctionFrame (update' currFrame x v)
 
 
 
       (*FUNCTION: Declares a new given identifier in current environment frame, and if given, to a value.*)
       let newVarDec (currFrame : t) (x : Ast.Id.t) (v : Value.t) : t =
               match currFrame with
               | ReturnFrame _ -> failwith @@ "Variable Declaration in a Return Frame"
               | FunctionFrame currFrame' -> match currFrame' with
                                               | [] -> failwith @@ "VarDec in EmptyFrame"
                                               | y :: ys -> if IdMap.mem x y then
                                                                  raise @@ MultipleDeclaration x
                                                            else
                                                                  FunctionFrame ((IdMap.add x v y) :: ys)
       (*FUNCTION: Adds a new environment frame to the stack of environment frames*)
       let addBlock (currFrame : t) : t =
             match currFrame with
             | ReturnFrame _ ->  failwith @@ "Unimplemented 1"
             | FunctionFrame currFrame' -> match currFrame' with
                             | [] -> FunctionFrame (IdMap.empty :: [])
                             | y :: ys -> FunctionFrame (IdMap.empty :: y :: ys)
       (*FUNCTION: Removes top environment fram from stack of environment frames*)
       let removeBlock (currFrame : t) : t =
             match currFrame with
             | ReturnFrame _ ->  failwith @@ "Unimplemented 2"
             | FunctionFrame currFrame' -> match currFrame' with
                             | [] -> failwith @@ "No Block to Remove"
                             | _ :: ys -> FunctionFrame (ys)
       (*FUNCTION: Verifies to see if a given frame is an environment frame. Returns false if it is a Return Frame*)
       let isFuncFrame (currFrame : t) : bool =
             match currFrame with
             | FunctionFrame _ -> true
             | ReturnFrame _ -> false
 
       let newFuncFrame : t =
         FunctionFrame [IdMap.empty]
       (*FUNCTION: Creates new return frame with given value v*)
       let newReturnFrame (v : Value.t) : t =
         ReturnFrame v
 
       let empty : t = FunctionFrame [IdMap.empty]
 
 end
 
 (* Code for all the Functions stuff
 *)
 module FunMap = Map.Make(Ast.Id)
 
 module Fun = struct
 
     type t = (Ast.Id.t list * S.t list) FunMap.t
 
     (*FUNCTION: Identifies all function name identifiers given a program, and creates a map of all function identifiers with params and body*)
     let rec collectFun (l : Ast.Program.fundef list) (funMap : t) : t =
         match l with
         | [] -> funMap
         | (Ast.Program.FunDef (name, params, body)) :: xs ->  collectFun xs (FunMap.add name (params, body) funMap)
     (*FUNCTION: Calls top function with a given program on an empty module*)
     let collectFun (l : Ast.Program.fundef list) : t =
         collectFun l FunMap.empty
 
     (*FUNCTION: Given a map and a given function identifier, returns the params and body of function in the map with the corresponding identifier*)
     let findFunc (funMap : t) (x : Ast.Id.t) : (Ast.Id.t list * S.t list) option =
         try
             Some (FunMap.find x funMap)
         with
             | Not_found -> None
     (*HELPER: Adds all given identifiers and values to the Environment frame and binds them*)
     let rec initFun' (env : Env.t) (paramList : (Ast.Id.t * Value.t) list) : Env.t =
         match paramList with
             | [] -> env
             | (i, v) :: xs -> let env' = Env.newVarDec env i v in
                                                 initFun' env' xs
     (*FUNCTION: Initializes new environment frame separate from main frame for when function is called*)
     let initFun (paramList : (Ast.Id.t * Value.t) list) : Env.t =
         let env = Env.newFuncFrame in
             initFun' env paramList
 
 
 end
 
 (* FUNCTION binop: Matches values to the correct BINOP expression. Takes in BINOP operand and two values to compute operand on and returns its value t. *)
 let binop (op : E.binop) (v : Primval.t) (v' : Primval.t) : Primval.t =
   match (op, v, v') with
   | (E.Plus, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Int (n + n')
   | (E.Minus, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Int(n - n')
   | (E.Div, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Int(n / n')
   | (E.Times, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Int (n * n')
   | (E.And, Primval.V_Bool n, Primval.V_Bool n') -> Primval.V_Bool (n && n')
   | (E.Mod, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Int (n mod n')
   | (E.Or, Primval.V_Bool n, Primval.V_Bool n') -> Primval.V_Bool(n || n')
   | (E.Eq, Primval.V_Bool n, Primval.V_Bool n') -> Primval.V_Bool (n = n')
   | (E.Eq, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Bool(n = n')
   | (E.Le, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Bool (n <= n')
   | (E.Ge, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Bool (n >= n')
   | (E.Ne, Primval.V_Bool n, Primval.V_Bool n') -> Primval.V_Bool (n <> n')
   | (E.Ne, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Bool(n <> n')
   | (E.Lt, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Bool(n < n')
   | (E.Gt, Primval.V_Int n, Primval.V_Int n') -> Primval.V_Bool(n > n')
   | _ -> raise (TypeError ("Arg 1: " ^ Primval.to_string v ^ "Arg 2:" ^ Primval.to_string v' ^ "Operator: Unknown"))
 
 (*HELPER: given a list of identifiers and values, returns list of tuples with identifier and value*)
 let rec zip (l1 : Ast.Id.t list) (l2 : Value.t list) : (Ast.Id.t * Value.t) list =
   match l1, l2 with
   | [],[] -> []
   | x::xs, y::ys -> (x, y) :: zip xs ys
   | _ -> failwith @@ "No lists"
 
   (* FUNCTION eval (recursive): Evaluates Expressions. Takes in environment sigma, expression e and function t (for Call) and returns the expression's value and updated environment. *)
   let rec eval (sigma : Env.t) (e : E.t) (f: Fun.t) (l: SecurityLabel.t ): Value.t * Env.t =
     match e with
     (* DONE: Variable Lookup. *)
     | E.Var x -> 
      let Value.Val(v, l') = Env.lookup sigma x in
      let lnew = SecurityLabel.compare l l' in
          (Value.Val(v, lnew ), sigma)
     (* DONE: Integer expression.*)
     | E.Num n -> (Value.Val (Primval.V_Int n, l), sigma)
     (* Boolean expression. *)
     | E.Bool b -> (Value.Val(Primval.V_Bool b, l), sigma)
     (* DONE: String expression. *)
     | E.Str s -> (Value.Val(Primval.V_Str s, l) , sigma)
     (* DONE: Calls BINOP expression. Makes recursive call to evaluate each internal expression. *)
     | E.Binop (op, e1, e2) ->
       let (Value.Val (v1, l1), sigma1) = eval sigma e1 f l in
       let (Value.Val (v2, l2), sigma2) = eval sigma1 e2 f l in
       let newval = binop op v1 v2 in
       let lnew = SecurityLabel.compare l1 l2 in
       (Value.Val (newval, lnew), sigma2)
     (* DONE: Assign value in expression e to x. *)
     | E.Assign (x, e) ->
       let (Value.Val (v1,l1), sigma') = eval sigma e f l in
        (match (l,l1) with 
        |(SecurityLabel.High, SecurityLabel.Low) -> failwith "Assign Error"
        |_ -> let sigma2 = Env.update sigma' x (Value.Val (v1, SecurityLabel.compare l l1)) in
        (Value.Val (v1, SecurityLabel.compare l l1), sigma2)) 
     (* DONE: Not operator (switches Boolean expressions).  *)
     | E.Not e ->
       let (Value.Val (v1,l1), sigma') = eval sigma e f l in
       (match v1 with
        | Primval.V_Bool b -> (Value.Val(Primval.V_Bool (not b), l1) , sigma')
        | _ -> failwith "Type Error")
     (* DONE: Negative of expression e. First evaluates e and then returns its negative. *)
     | E.Neg e ->
       let (Value.Val (v1, l1), sigma') = eval sigma e f l in
       (match v1 with
        | Primval.V_Int n -> (Value.Val(Primval.V_Int (-n), l1) , sigma')
        | _ -> failwith "Type Error")
     (*DONE: CALL MATCH: Given a list of expressions, evaluates all expressions. Then matches given function identifier and returns list of params and the body.
        Zips param identifiers with values from expression list, and executes body in a new frame*)
     | E.Call (func, llist) ->
       let (vl, sigma') = eval_all llist sigma f l in
       (match Fun.findFunc f func with
       | None -> (try
                     let v = Api.do_call func vl in
                     (v, sigma')
                 with
                     | _ -> raise @@ UndefinedFunction func)
       | Some (xl, sl) ->
       let xvl = zip xl vl in
       let sigma2 = Fun.initFun xvl in
       (match exec_stm (S.Block sl) sigma2 f l with
        | ReturnFrame v -> (v, sigma')
        | _ -> failwith "Not a return frame"))
 
   (*HELPER: Given a list of expressions and an environment, returns list of values from expressions and updated environment frame*)
   and eval_all(el: E.t list) (sigma: Env.t) (f: Fun.t)(l: SecurityLabel.t ): Value.t list * Env.t =
   match el with
   | [] -> ([], sigma)
   | x :: xs ->
     let (v, sigma') = eval sigma x f l  in
     let (vs, sigma2) = eval_all xs sigma' f l in
     (v::vs, sigma2)
 
     (* FUNCTION exec_stm (recursive): Executes statements. Takes in statement stm, environment sigma, and function t and returns the updated environment. *)
     and exec_stm (stm : S.t) (sigma : Env.t) (f : Fun.t) (l : SecurityLabel.t) : Env.t =
       match stm with
       (* Skip statement. Case where there is no "else" within an if statement. *)
       | S.Skip -> sigma
       (* Variable declaration. Accounts for multiple declarations within a line (therefore taking a list). Evaluates expression and assigns values to its variables (recursively within list). *)
       | S.VarDec llist ->
         (match llist with
          | [] -> sigma
          | (var, e) :: xs ->
           match e with
           | Some e' ->
             let (v, sigma') = eval sigma e' f l in
             let sigma2 = Env.newVarDec sigma' var v in
             exec_stm (S.VarDec xs) sigma2 f l
           | None -> let sigma' = Env.newVarDec sigma var (Value.Val(Primval.V_None, l)) in
             exec_stm (S.VarDec xs) sigma' f l)
       (* Evaluates expression within statement. *)
       | S.Expr e ->
         let (_, sigma') = eval sigma e f in
         sigma'
       (*BLOCK MATCH: Given a list of statements, adds a new environment to environment stack, evaluates the whole list of statements under the current frame,
          and removes top environment frame when finished*)
       | S.Block l -> (let sigma' = Env.addBlock sigma in
                         let sigma2 = stm_list l sigma' f in
                             match sigma2 with
                             | Env.FunctionFrame _ -> Env.removeBlock sigma2
                             | Env.ReturnFrame _ -> sigma2)
       (* Executes if statements. Evaluates boolean expression e (accounting for type errors), and recursively executes statement based on its result. *)
       | S.If (e, s0, s1) ->
         let (Value.Val (v1,l1), sigma') = eval sigma e f l in
         (match v1 with
          | Primval.V_Bool true -> exec_stm s0 sigma' f l1
          | Primval.V_Bool false -> exec_stm s1 sigma' f l1
          | _ -> raise (TypeError "Non-boolean value in if condition"))
       (*WHILE MATCH: Given an expression and a body, evaluates expression and continues to evaluate body until expression returns false*)
       | S.While (e, s) -> loop e s sigma f
       (* Return case with value from expression e. Calls helper function to create a return frame with value v. *)
       | S.Return Some e ->
         let (Value.Val (v1,l1), _) = eval sigma e f l in
         Env.newReturnFrame Value.Val (v1,l1)
       (* Return case with no value. Creates new return frame with None. *)
       | S.Return None -> Env.newReturnFrame Value.V_None
     (*HELPER: For while loops. Evaluates given expression under the environment frame. If false then returns updated frame. If true then adds a new environment frame onto frame stack.
        Then checks evaluates the block. If a return frame is given, we return said return frame. Else, we evaluate the loop again. After finished, we remove top
        environment frame or return return frame.*)
     and loop (e : E.t) (s : S.t) (sigma : Env.t) (f : Fun.t) : Env.t =
       let (v, sigma') = eval sigma e f in
       match v with
       | Value.V_Bool false -> sigma'
       | Value.V_Bool true ->
        ( match s with
         | S.Block s' -> let sigma2 = Env.addBlock sigma' in
         let sigma3 = stm_list s' sigma2 f in
         (match sigma3 with
          | Env.ReturnFrame _ -> sigma3
          | Env.FunctionFrame _ -> let sigma4 = loop e s sigma3 f in
                                     (match sigma4 with
                                     | Env.FunctionFrame _ ->  Env.removeBlock sigma4
                                     | Env.ReturnFrame _ -> sigma4))
         | _ -> let sigma2 = exec_stm s sigma' f in
               (match sigma2 with
                  | Env.ReturnFrame _ -> sigma2
                  | Env.FunctionFrame _ -> loop e s sigma2 f))
       | _ -> raise (TypeError "Non-boolean value in while condition")
 
 
     (*HELPER: Given list of statements, evalates each statement under environment given and updated.*)
     and stm_list (ss : S.t list) (sigma : Env.t) (f : Fun.t) : Env.t =
       match ss with
       | [] -> sigma
       | s :: rest ->
         let sigma' = exec_stm s sigma f in
         (match sigma' with
          | Env.FunctionFrame _ -> stm_list rest sigma' f
          | Env.ReturnFrame _ -> sigma')
     
 
     
 (* exec p :  execute the program p according to the operational semantics
  * provided as a handout.
  *)
   let exec (stm : Ast.Program.t) : unit =
     match stm with
       | Ast.Program.Pgm(stm') ->
         let f = Fun.collectFun stm' in
         let funName = "main" in
         match Fun.findFunc f funName with
         | None -> ()
         | Some (_, stmt_list) ->
             let env = Env.newFuncFrame in
             let _ = exec_stm (S.Block stmt_list) env f in
                 ()
 
 