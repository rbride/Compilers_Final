(* There are two changes made in here from 'fer-de-lance' assignment (assignment 8).
   1) Change/reorganize naive_stack_allocation. 

   [***Note*** We are essentially changing our look-up environment into a hashmap!!!

   Previously, all the bindings were put inside one list of global environment
   which simply combined all mappings of variable to stack location in one simple list.
   This made it difficult to distinguish the different stack frame environments.]

   [*****Note***** Each function has a unique stack frame environment. Note that the entire expression is considered the 
   initial function our_code_starts_here and therefore also has a unique stack frame environment; I.e. when compiling 
   the starting expression starting_e, what we are actually doing is implicitly calling 'ocsh(arg1, arg2)' whose function 
   body is starting_e.

   A stack environment is a mapping of all the variables (arguments, local variables, free-variables) to the  
   location in the stack frame which is created by when this function is called.
 
        The total set of stack environments in a given expression = stack env of whole expression + stack env of f1 +
        ... + stack env of fn. 
        
        So there are total of n + 1 stack environments in a given expression where n is the number of function declarations.

        *** DON'T CONFLATE 'STACK ENVIRONMENT' AND 'STACK FRAME'. 
        A 'STACK FRAME' OF A FUNCTION IS AN ACTUAL BLOCK OF MEMORY CREATED ON THE STACK WHEN A FUNCTION IS CALLED.
        A 'STACK ENVIRONMENT' IS THE SET OF ALL MAPPINGS OF VARIABLES OF A FUNCTION TO LOOK UP FROM THE STACK FRAME 
        WHEN THE FUNCTION IS CALLED. 

        IN A SENSE YOU CAN THINK OF 'STACK ENVIRONMENT' AS THE "FORM" AND 'STACK FRAME' AS THE INSTANCE. THERE CAN BE 1 BILLION
        STACK FRAMES CREATED AND DESTROYED FOR WHEN A FUNCTION IS CALLED 1 BILLION TIMES, BUT THERE WILL ALWAYS BE ONLY 1 
        STACK ENVIRONMENT OF THIS FUNCTION.]

  [****Note**** Recap: Given that every function declaration is desugared into a binding whose bound expression is a lambda expression,
   and the value of a lambda expression is its closure (tuple like structure on the heap), this was the semantics change
   of function call:

   Original semantics: function call = look up fun name to get fun body + provide arguments to fun body
        Implementation: 1) Create the stack frame of the function. 
                        2) arguments are positive offsets of RBP, 
                        3) local vars of function body are negative offsets of RBP.


   Now: function call = look up binding name of lambda expression + provide closure of lambda expression as first argument 
                        + provide regular arguments of lambda expression 
        Implementation: 1) Make a new stack frame for this lambda, 2) closure is first positive offset of RBP, 3) arguments of 
                        lambda expressions are second ~ nth positive offset of RBP, 4) free vars of lambda body are
                        first set to put on as the negative offsets of RBP, 5) local vars of lambda body are second
                        set to put on as the negative offsets of RBP. 

   I.e. previously, if we had stackframe 1, ..., stack frame n, where stackframe i + 1 lies on top of stack frame i (i.e.
    stackframe i+1 corresponds to lambda / closure called from function call or expression corresponding to stackframe i), the
   our global environment was just a simple list structure that concatenates all the bindings of these environments:

    global env = [(sfn_b1: loc_sfn_b1), .., (sfn_bm: loc_sfn_bm), ..., (sf1_b1: loc_sf1_b1), ..., (sf1_bm': loc_sf1_bm')]

  The structure of global env does not quite capture the distinctness of the stack frames. To fix this, we change the type
  of our global env to be a list of bindings of list of bindings. 
  
  I.e. ALL WE ARE DOING IS CREATING A HASH MAP WHERE EACH KEY OF THE HASHMAP IS THE REFERENCE TO A UNIQUE STACK FRAME, AND 
       THE VALUE OF EACH KEY IS THE ENVIRONMENT REPRESENTING THAT UNIQUE STACK FRAME. 

       Each sub-env corresponding to a stack frame is a list of bindings, and this sub-env associated with the bind name
       of the lambda/closure whose call creates this stack frame. And this mapping of lambda/closure name to stack frame 
       sub environment is an element of the new global env, as such:

    Option 1) Use the bind name of the lambda/closure (i.e. name of lambda/closure) as key of the stack frame env corresponding
    to its call. One inconvenience with this is that I have to change ANF implementation, since not every closure has a name. 

    Signature change => naive_stack_allocation : tag aprogram -> (tag aprogram * arg name_envt name_envt)
    Return type 'arg name_envt name_envt' looks like:  

    new global env = [(name_of_lambda_of_sfn: [(sfn_var1: sfn_var1), .., (sfn_varm: loc_sfn_varm)]), 
                          ..., 
                         (name_of_lambda_of_sf1: [(sf1_var1: loc_sf1_var1), ..., (sf1_varm': loc_sf1_varm')])]

    Option 2) Use the tag associated with the lambda/closure as the key of the stack frame env corresponding to its call.
    This approach also has risks, since it requires that you never re-tag the AST once you’ve produced this environment.

    Signature change => naive_stack_allocation : tag aprogram -> (tag aprogram * arg name_envt tag_envt)
    Return type 'arg name_envt tag_envt' looks like:  

    new global env = [(tag_of_lambda_of_sfn: [(sfn_var1: loc_sfn_var1), .., (sfn_varm: loc_sfn_varm)]), 
                          ..., 
                         (tag_of_lambda_of_sf1: [(sf1_var1: loc_sf1_var1), ..., (sf1_varm': loc_sf1_varm')])]    

    Option 2 is the choice of implementation. This choice requires us to only change naive_stack_allocation, and 
    the compile phase, while leaving all other phases as they were before.] 

    [****Note**** There is ONE KIND OF EXCEPTION IN our stack environment where a given environment contains mapping(s) of
    variable -> code_ptr/label rather than variable -> stack location. This environment is the initial environment
    of our_code_starts_here for where its free variables which are native functions (print, equal, input) are mapped to the 
    assembly labels (i.e. code ptrs) '?print', '?equal', and '?input'. 

    I.e. the stack environment of ocsh is, at the very least: 
    (tag_0, [($heap, [RBP + 24]), ($size, [RBP + 32]), // These map the argument variables of ocsh to stack locations 
             (print, "?print"), (equal, "?equal"), (input, "?input") // These map free vars of ocsh to code_ptrs/labels
             (print_bind_tag#, [RBP - 8]), (equal_bind_tag#, [RBP - 16]), (input_bind_tag#, [RBP - 24]) 
             // These map the default let bindings of the lambdas of print, equal, input, to stack locations
             ])
    
    print_binding_name])
    
    
    where the mapping is not of (variable, stack location),
    but rather the mapping 
    ]
    [*****Note***** Native C functions and other globals - '?error', '?print_stack', '?try_gc', '?naive_print_heap', '?HEAP',
    '?HEAP_END', '?set_stack_bottom' - are not in the stack environment because they are never called by the program directly
    as concrete syntax. 
    
    The only C functions that are called directly as concrete syntax are '?print' - e.g concrete syntax "print(1)",
    '?equal' - e.g. concrete syntax "equal(3, 3)", and '?input' - e.g. concrete syntax "input()". 

    E.g. while we can have our program be 'let x = 1 in print(1)' which contains a call to the native C function '?print',
    we will never have for example, a program 'let x = 1 in try_gc(1)'. 
    Note that while our program can call '?print_stack' directly via something like 'let x = 1 in printStack(1)', 'printStack'
    is not a free variable but we rather defined as a CPrim1 variant.]
    ]
*)

open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors

module StringSet = Set.Make(String)


type 'a name_envt = (string * 'a) list
type 'a tag_envt  = (tag * 'a) list


let print_env env how =
  debug_printf "Env is\n";
  List.iter (fun (id, bind) -> debug_printf "  %s -> %s\n" id (how bind)) env;;


let const_true       = HexConst(0xFFFFFFFFFFFFFFFFL)
let const_false      = HexConst(0x7FFFFFFFFFFFFFFFL)
let bool_mask        = HexConst(0x8000000000000000L)
let bool_tag         = 0x0000000000000007L
let bool_tag_mask    = 0x0000000000000007L
let num_tag          = 0x0000000000000000L
let num_tag_mask     = 0x0000000000000001L
let closure_tag      = 0x0000000000000005L
let closure_tag_mask = 0x0000000000000007L
let tuple_tag        = 0x0000000000000001L
let tuple_tag_mask   = 0x0000000000000007L
let nil_val          = HexConst(tuple_tag)

let err_COMP_NOT_NUM     = 1L
let err_ARITH_NOT_NUM    = 2L
let err_LOGIC_NOT_BOOL   = 3L
let err_IF_NOT_BOOL      = 4L
let err_OVERFLOW         = 5L
let err_GET_NOT_TUPLE    = 6L
let err_GET_LOW_INDEX    = 7L
let err_GET_HIGH_INDEX   = 8L
let err_GET_NOT_NUM      = 9L
let err_NIL_DEREF        = 10L
let err_OUT_OF_MEMORY    = 11L
let err_SET_NOT_TUPLE    = 12L
let err_SET_LOW_INDEX    = 13L
let err_SET_NOT_NUM      = 14L
let err_SET_HIGH_INDEX   = 15L
let err_CALL_NOT_CLOSURE = 16L
let err_CALL_ARITY_ERR   = 17L
let err_INDEX_NOT_NUMBER = 18L
let err_LET_TUPLE_MISMATCH = 19L

let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos);;

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]
let heap_reg = R15
let scratch_reg = R11

(* you can add any functions or data defined by the runtime here for future use *)
let initial_val_env = [
  ("print", (Native, 1));
  ("input", (Native, 0));
  ("equal", (Native, 2));
];;

let prim_bindings = [];;
let native_fun_bindings = [
  ("print", (Native, 1));
  ("input", (Native, 0));
  ("equal", (Native, 2));
  ];;

let initial_fun_env = prim_bindings @ native_fun_bindings;;

(* You may find some of these helpers useful *)

let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y,v)::rest ->
     if y = x then v else find rest x

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq(e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ALetRec(binds, body, _) ->
       (List.length binds) + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf(_, t, f, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in helpA e

(* Returns the stack-index (in words) of the deepest stack index used for any 
   of the variables in this expression *)
let rec deepest_stack e env =
  let rec helpA e =
    match e with
    | ALet(name, bind, body, _) -> List.fold_left max 0 [name_to_offset name; helpC bind; helpA body]
    | ALetRec(binds, body, _) -> List.fold_left max (helpA body) (List.map (fun (name, _) -> name_to_offset name) binds)
    | ASeq(first, rest, _) -> max (helpC first) (helpA rest)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf(c, t, f, _) -> List.fold_left max 0 [helpI c; helpA t; helpA f]
    | CPrim1(_, i, _) -> helpI i
    | CPrim2(_, i1, i2, _) -> max (helpI i1) (helpI i2)
    | CApp(_, args, _, _) -> List.fold_left max 0 (List.map helpI args)
    | CTuple(vals, _) -> List.fold_left max 0 (List.map helpI vals)
    | CGetItem(t, _, _) -> helpI t
    | CSetItem(t, _, v, _) -> max (helpI t) (helpI v)
    | CLambda(args, body, _) ->
      0
      (* let new_env = (List.mapi (fun i a -> (a, RegOffset(word_size * (i + 3), RBP))) args) @ env in
      deepest_stack body new_env *)
    | CImmExpr i -> helpI i
  and helpI i =
    match i with
    | ImmNil _ -> 0
    | ImmNum _ -> 0
    | ImmBool _ -> 0
    | ImmId(name, _) -> name_to_offset name
  and name_to_offset name =
    match (find env name) with
    | RegOffset(bytes, RBP) -> bytes / (-1 * word_size) (* negative because stack direction *)
    | _ -> 0
  in max (helpA e) 0 (* if only parameters are used, helpA might return a negative value *)
;;

let rec replicate x i =
  if i = 0 then []
  else x :: (replicate x (i - 1))


let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs
;;

let rec find_opt (env : 'a name_envt) (elt: string) : 'a option =
  match env with
  | [] -> None
  | (x, v) :: rst -> if x = elt then Some(v) else find_opt rst elt
;;
                             
(* Prepends a list-like env onto an name_envt *)
let merge_envs list_env1 list_env2 =
  list_env1 @ list_env2
;;
(* Combines two name_envts into one, preferring the first one *)
let prepend env1 env2 =
  let rec help env1 env2 =
    match env1 with
    | [] -> env2
    | ((k, _) as fst)::rst ->
      let rst_prepend = help rst env2 in
      if List.mem_assoc k env2 then rst_prepend else fst::rst_prepend
  in
  help env1 env2
;;

let env_keys e = List.map fst e;;

(* Scope_info stores the location where something was defined,
   and if it was a function declaration, then its type arity and argument arity *)
type scope_info = (sourcespan * int option * int option)
let is_well_formed (p : sourcespan program) : (sourcespan program) fallible =
  let rec wf_E e (env : scope_info name_envt) =
    debug_printf "In wf_E: %s\n" (ExtString.String.join ", " (env_keys env));
    match e with
    | ESeq(e1, e2, _) -> wf_E e1 env @ wf_E e2 env
    | ETuple(es, _) -> List.concat (List.map (fun e -> wf_E e env) es)
    | EGetItem(e, idx, pos) ->
       wf_E e env @ wf_E idx env
    | ESetItem(e, idx, newval, pos) ->
       wf_E e env @ wf_E idx env @ wf_E newval env
    | ENil _ -> []
    | EBool _ -> []
    | ENumber(n, loc) ->
       if n > (Int64.div Int64.max_int 2L) || n < (Int64.div Int64.min_int 2L) then
         [Overflow(n, loc)]
       else
         []
    | EId (x, loc) -> if (find_one (List.map fst env) x) then [] else [UnboundId(x, loc)]
    | EPrim1(_, e, _) -> wf_E e env
    | EPrim2(_, l, r, _) -> wf_E l env @ wf_E r env
    | EIf(c, t, f, _) -> wf_E c env @ wf_E t env @ wf_E f env
    | ELet(bindings, body, _) ->
       let rec find_locs x (binds : 'a bind list) : 'a list =
         match binds with
         | [] -> []
         | BBlank _::rest -> find_locs x rest
         | BName(y, _, loc)::rest ->
            if x = y then loc :: find_locs x rest
            else  find_locs x rest
         | BTuple(binds, _)::rest -> find_locs x binds @ find_locs x rest in
       let rec find_dupes (binds : 'a bind list) : exn list =
         match binds with
         | [] -> []
         | (BBlank _::rest) -> find_dupes rest
         | (BName(x, _, def)::rest) -> (List.map (fun use -> DuplicateId(x, use, def)) (find_locs x rest)) @ (find_dupes rest)
         | (BTuple(binds, _)::rest) -> find_dupes (binds @ rest) in
       let dupeIds = find_dupes (List.map (fun (b, _, _) -> b) bindings) in
       let rec process_binds (rem_binds : 'a bind list) (env : scope_info name_envt) =
         match rem_binds with
         | [] -> (env, [])
         | BBlank _::rest -> process_binds rest env
         | BTuple(binds, _)::rest -> process_binds (binds @ rest) env
         | BName(x, allow_shadow, xloc)::rest ->
            let shadow =
              if allow_shadow then []
              else match find_opt env x with
                   | None -> []
                   | Some (existing, _, _) -> [ShadowId(x, xloc, existing)] in
            let new_env = (x, (xloc, None, None))::env in
            let (newer_env, errs) = process_binds rest new_env in
            (newer_env, (shadow @ errs)) in
       let rec process_bindings bindings (env : scope_info name_envt) =
         match bindings with
         | [] -> (env, [])
         | (b, e, loc)::rest ->
            let errs_e = wf_E e env in
            let (env', errs) = process_binds [b] env in
            let (env'', errs') = process_bindings rest env' in
            (env'', errs @ errs_e @ errs') in
       let (env2, errs) = process_bindings bindings env in
       dupeIds @ errs @ wf_E body env2
    | EApp(func, args, native, loc) ->
       let rec_errors = List.concat (List.map (fun e -> wf_E e env) (func :: args)) in
       (match func with
        | EId(funname, _) -> 
           (match (find_opt env funname) with
            | Some(_, _, Some arg_arity) ->
               let actual = List.length args in
               if actual != arg_arity then [Arity(arg_arity, actual, loc)] else []
            | _ -> [])
        | _ -> [])
       @ rec_errors
    | ELetRec(binds, body, _) ->
       let nonfuns = List.find_all (fun b -> match b with | (BName _, ELambda _, _) -> false | _ -> true) binds in
       let nonfun_errs = List.map (fun (b, _, where) -> LetRecNonFunction(b, where)) nonfuns in

     
       let rec find_locs x (binds : 'a bind list) : 'a list =
         match binds with
         | [] -> []
         | BBlank _::rest -> find_locs x rest
         | BName(y, _, loc)::rest ->
            if x = y then loc :: find_locs x rest
            else  find_locs x rest
         | BTuple(binds, _)::rest -> find_locs x binds @ find_locs x rest in
       let rec find_dupes (binds : 'a bind list) : exn list =
         match binds with
         | [] -> []
         | (BBlank _::rest) -> find_dupes rest
         | (BName(x, _, def)::rest) -> List.map (fun use -> DuplicateId(x, use, def)) (find_locs x rest)
         | (BTuple(binds, _)::rest) -> find_dupes (binds @ rest) in
       let dupeIds = find_dupes (List.map (fun (b, _, _) -> b) binds) in
       let rec process_binds (rem_binds : sourcespan bind list) (env : scope_info name_envt) =
         match rem_binds with
         | [] -> (env, [])
         | BBlank _::rest -> process_binds rest env
         | BTuple(binds, _)::rest -> process_binds (binds @ rest) env
         | BName(x, allow_shadow, xloc)::rest ->
            let shadow =
              if allow_shadow then []
              else match (find_opt env x) with
                   | None -> []
                   | Some (existing, _, _) -> if xloc = existing then [] else [ShadowId(x, xloc, existing)] in
            let new_env = (x, (xloc, None, None))::env in
            let (newer_env, errs) = process_binds rest new_env in
            (newer_env, (shadow @ errs)) in

       let (env, bind_errs) = process_binds (List.map (fun (b, _, _) -> b) binds) env in
       
       let rec process_bindings bindings env =
         match bindings with
         | [] -> (env, [])
         | (b, e, loc)::rest ->
            let (env, errs) = process_binds [b] env in
            let errs_e = wf_E e env in
            let (env', errs') = process_bindings rest env in
            (env', errs @ errs_e @ errs') in
       let (new_env, binding_errs) = process_bindings binds env in

       let rhs_problems = List.map (fun (_, rhs, _) -> wf_E rhs new_env) binds in
       let body_problems = wf_E body new_env in
       nonfun_errs @ dupeIds @ bind_errs @ binding_errs @ (List.flatten rhs_problems) @ body_problems
    | ELambda(binds, body, _) ->
       let rec dupe x args =
         match args with
         | [] -> None
         | BName(y, _, loc)::_ when x = y -> Some loc
         | BTuple(binds, _)::rest -> dupe x (binds @ rest)
         | _::rest -> dupe x rest in
       let rec process_args rem_args =
         match rem_args with
         | [] -> []
         | BBlank _::rest -> process_args rest
         | BName(x, _, loc)::rest ->
            (match dupe x rest with
             | None -> []
             | Some where -> [DuplicateId(x, where, loc)]) @ process_args rest
         | BTuple(binds, loc)::rest ->
            process_args (binds @ rest)
       in
       let rec flatten_bind (bind : sourcespan bind) : (string * scope_info) list =
         match bind with
         | BBlank _ -> []
         | BName(x, _, xloc) -> [(x, (xloc, None, None))]
         | BTuple(args, _) -> List.concat (List.map flatten_bind args) in
       (process_args binds) @ wf_E body (merge_envs (List.concat (List.map flatten_bind binds)) env)
  and wf_D d (env : scope_info name_envt) (tyenv : StringSet.t) =
    match d with
    | DFun(_, args, body, _) ->
       let rec dupe x args =
         match args with
         | [] -> None
         | BName(y, _, loc)::_ when x = y -> Some loc
         | BTuple(binds, _)::rest -> dupe x (binds @ rest)
         | _::rest -> dupe x rest in
       let rec process_args rem_args =
         match rem_args with
         | [] -> []
         | BBlank _::rest -> process_args rest
         | BName(x, _, loc)::rest ->
            (match dupe x rest with
             | None -> []
             | Some where -> [DuplicateId(x, where, loc)]) @ process_args rest
         | BTuple(binds, loc)::rest ->
            process_args (binds @ rest)
       in
       let rec arg_env args (env : scope_info name_envt) =
         match args with
         | [] -> env
         | BBlank _ :: rest -> arg_env rest env
         | BName(name, _, loc)::rest -> (name, (loc, None, None))::(arg_env rest env)
         | BTuple(binds, _)::rest -> arg_env (binds @ rest) env in
       (process_args args) @ (wf_E body (arg_env args env))
  and wf_G (g : sourcespan decl list) (env : scope_info name_envt) (tyenv : StringSet.t) =
    let add_funbind (env : scope_info name_envt) d =
      match d with
      | DFun(name, args, _, loc) ->
         (name, (loc, Some (List.length args), Some (List.length args)))::env in
    let env = List.fold_left add_funbind env g in
    let errs = List.concat (List.map (fun d -> wf_D d env tyenv) g) in
    (errs, env)
  in
  match p with
  | Program(decls, body, _) ->
     let initial_env = initial_val_env in
     let initial_env = List.fold_left
                          (fun env (name, (_, arg_count)) -> (name, (dummy_span, Some arg_count, Some arg_count))::env)
     []
     initial_env in
     let rec find name (decls : 'a decl list) =
       match decls with
       | [] -> None
       | DFun(n, args, _, loc)::rest when n = name -> Some(loc)
       | _::rest -> find name rest in
     let rec dupe_funbinds decls =
       match decls with
       | [] -> []
       | DFun(name, args, _, loc)::rest ->
          (match find name rest with
          | None -> []
          | Some where -> [DuplicateFun(name, where, loc)]) @ dupe_funbinds rest in
     let all_decls = List.flatten decls in
     let initial_tyenv = StringSet.of_list ["Int"; "Bool"] in
     let help_G (env, exns) g =
       let (g_exns, funbinds) = wf_G g env initial_tyenv in
       (List.fold_left (fun xs x -> x::xs) env funbinds, exns @ g_exns) in
     let (env, exns) = List.fold_left help_G (initial_env, dupe_funbinds all_decls) decls in
     debug_printf "In wf_P: %s\n" (ExtString.String.join ", " (env_keys env));
     let exns = exns @ (wf_E body env)
     in match exns with
        | [] -> Ok p
        | _ -> Error exns
;;

(* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; DESUGARING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *)

let desugar (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    (fun name ->
      next := !next + 1;
      sprintf "%s_%d" name (!next)) in
  let rec helpP (p : sourcespan program) =
    match p with
    | Program(decls, body, tag) ->
       (* This particular desugaring will convert declgroups into ELetRecs *)
       let merge_sourcespans ((s1, _) : sourcespan) ((_, s2) : sourcespan) : sourcespan = (s1, s2) in
       let wrap_G g body =
         match g with
         | [] -> body
         | f :: r ->
            let span = List.fold_left merge_sourcespans (get_tag_D f) (List.map get_tag_D r) in
            ELetRec(helpG g, body, span) in
       Program([], List.fold_right wrap_G decls (helpE body), tag)
  and helpG g =
    List.map helpD g
  and helpD d =
    match d with
    | DFun(name, args, body, tag) ->
       let helpArg a =
         match a with
         | BTuple(_, tag) ->
            let name = gensym "argtup" in
            let newbind = BName(name, false, tag) in
            (newbind, [(a, EId(name, tag), tag)])
         | _ -> (a, []) in
       let (newargs, argbinds) = List.split (List.map helpArg args) in
       let newbody = ELet(List.flatten argbinds, body, tag) in
       (BName(name, false, tag), ELambda(newargs, helpE newbody, tag), tag)
  and helpBE bind =
    let (b, e, btag) = bind in
    let e = helpE e in
    match b with
    | BTuple(binds, ttag) ->
       (match e with
        | EId _ ->
           expandTuple binds ttag e
        | _ ->
           let newname = gensym "tup" in
           (BName(newname, false, ttag), e, btag) :: expandTuple binds ttag (EId(newname, ttag)))
    | _ -> [(b, e, btag)]
  and expandTuple binds tag source : sourcespan binding list =
    let tupleBind i b =
      match b with
      | BBlank btag -> []
      | BName(_, _, btag) ->
        [(b, EGetItem(source, ENumber(Int64.of_int(i), dummy_span), tag), btag)]
      | BTuple(binds, tag) ->
          let newname = gensym "tup" in
          let newexpr = EId(newname, tag) in
          (BName(newname, false, tag), EGetItem(source, ENumber(Int64.of_int(i), dummy_span), tag), tag) :: expandTuple binds tag newexpr
    in
    let size_check = EPrim2(CheckSize, source, ENumber(Int64.of_int(List.length binds), dummy_span), dummy_span) in
    let size_check_bind = (BBlank(dummy_span), size_check, dummy_span) in
    size_check_bind::(List.flatten (List.mapi tupleBind binds))
  and helpE e =
    match e with
    | ESeq(e1, e2, tag) -> ELet([(BBlank(tag), helpE e1, tag)], helpE e2, tag)
    | ETuple(exprs, tag) -> ETuple(List.map helpE exprs, tag)
    | EGetItem(e, idx, tag) -> EGetItem(helpE e, helpE idx, tag)
    | ESetItem(e, idx, newval, tag) -> ESetItem(helpE e, helpE idx, helpE newval, tag)
    | EId(x, tag) -> EId(x, tag)
    | ENumber(n, tag) -> ENumber(n, tag)
    | EBool(b, tag) -> EBool(b, tag)
    | ENil(t, tag) -> ENil(t, tag)
    | EPrim1(op, e, tag) ->
       EPrim1(op, helpE e, tag)
    | EPrim2 (And, e1, e2, s) ->
        EIf
          (EPrim1 (Not, helpE e1, s), EBool (false, s), EPrim1 (Not, EPrim1 (Not, helpE e2, s), s), s)
    | EPrim2 (Or, e1, e2, s) ->
        EIf (EPrim1 (Not, helpE e1, s), EPrim1 (Not, EPrim1 (Not, helpE e2, s), s), EBool (true, s), s)
    | EPrim2(op, e1, e2, tag) ->
       EPrim2(op, helpE e1, helpE e2, tag)
    | ELet(binds, body, tag) ->
       let newbinds = (List.map helpBE binds) in
       List.fold_right (fun binds body -> ELet(binds, body, tag)) newbinds (helpE body)
    | ELetRec(bindexps, body, tag) ->
       (* ASSUMES well-formed letrec, so only BName bindings *)
       let newbinds = (List.map (fun (bind, e, tag) -> (bind, helpE e, tag)) bindexps) in
       ELetRec(newbinds, helpE body, tag)
    | EIf(cond, thn, els, tag) ->
       EIf(helpE cond, helpE thn, helpE els, tag)
    | EApp(name, args, native, tag) ->
       EApp(helpE name, List.map helpE args, native, tag)
    | ELambda(binds, body, tag) ->
       let expandBind bind =
         match bind with
         | BTuple(_, btag) ->
            let newparam = gensym "tuparg" in
            (BName(newparam, false, btag), helpBE (bind, EId(newparam, btag), btag))
         | _ -> (bind, []) in
       let (params, newbinds) = List.split (List.map expandBind binds) in
       let newbody = List.fold_right (fun binds body -> ELet(binds, body, tag)) newbinds (helpE body) in
       ELambda(params, newbody, tag)

  in helpP p
;;

(* ASSUMES desugaring is complete *)
let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with
    | Program(decls, body, tag) ->
       Program(List.map (fun group -> List.map (helpD env) group) decls, helpE env body, tag)
  and helpD env decl =
    match decl with
    | DFun(name, args, body, tag) ->
       let (newArgs, env') = helpBS env args in
       DFun(name, newArgs, helpE env' body, tag)
  and helpB env b =
    match b with
    | BBlank tag -> (b, env)
    | BName(name, allow_shadow, tag) ->
       let name' = sprintf "%s_%d" name tag in
       (BName(name', allow_shadow, tag), (name, name') :: env)
    | BTuple(binds, tag) ->
       let (binds', env') = helpBS env binds in
       (BTuple(binds', tag), env')
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> ([], env)
    | b::bs ->
       let (b', env') = helpB env b in
       let (bs', env'') = helpBS env' bs in
       (b'::bs', env'')
  and helpBG env (bindings : tag binding list) =
    match bindings with
    | [] -> ([], env)
    | (b, e, a)::bindings ->
       let (b', env') = helpB env b in
       let e' = helpE env e in
       let (bindings', env'') = helpBG env' bindings in
       ((b', e', a)::bindings', env'')
  and helpE env e =
    match e with
    | ESeq(e1, e2, tag) -> ESeq(helpE env e1, helpE env e2, tag)
    | ETuple(es, tag) -> ETuple(List.map (helpE env) es, tag)
    | EGetItem(e, idx, tag) -> EGetItem(helpE env e, helpE env idx, tag)
    | ESetItem(e, idx, newval, tag) -> ESetItem(helpE env e, helpE env idx, helpE env newval, tag)
    | EPrim1(op, arg, tag) -> EPrim1(op, helpE env arg, tag)
    | EPrim2(op, left, right, tag) -> EPrim2(op, helpE env left, helpE env right, tag)
    | EIf(c, t, f, tag) -> EIf(helpE env c, helpE env t, helpE env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EId(name, tag) ->
       (try
         EId(find env name, tag)
       with InternalCompilerError _ -> e)
    | EApp(func, args, native, tag) ->
       let funcImm = helpE env func in
       let call_type =
         (* TODO: If you want, try to determine whether func is a known function name, and if so,
            whether it's a Snake function or a Native function *)
         Snake in
        if native = Native then
       EApp(func, List.map (helpE env) args, native, tag)
        else 
       EApp(funcImm, List.map (helpE env) args, native, tag)
    | ELet(binds, body, tag) ->
       let (binds', env') = helpBG env binds in
       let body' = helpE env' body in
       ELet(binds', body', tag)
    | ELetRec(bindings, body, tag) ->
       let (revbinds, env) = List.fold_left (fun (revbinds, env) (b, e, t) ->
                                 let (b, env) = helpB env b in ((b, e, t)::revbinds, env)) ([], env) bindings in
       let bindings' = List.fold_left (fun bindings (b, e, tag) -> (b, helpE env e, tag)::bindings) [] revbinds in
       let body' = helpE env body in
       ELetRec(bindings', body', tag)
    | ELambda(binds, body, tag) ->
       let (binds', env') = helpBS env binds in
       let body' = helpE env' body in
       ELambda(binds', body', tag)
  in (rename [] p)
;;


(* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;; ANFING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *)


type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program([], body, _) -> AProgram(helpA body, ())
    | Program _ -> raise (InternalCompilerError "decls should have been desugared away")
  and helpC (e : tag expr) : (unit cexpr * unit anf_bind list) = 
    match e with
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (CPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (CPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (CIf(cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet([], body, _) -> helpC body
    | ELet((BBlank _, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BSeq exp_ans] @ body_setup)
    | ELet((BName(bind, _, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELetRec(binds, body, _) ->
       let processBind (bind, rhs, _) =
         match bind with
         | BName(name, _, _) -> (name, helpC rhs)
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a let-rec: %s"
                                             (string_of_bind bind))) in
       let (names, new_binds_setup) = List.split (List.map processBind binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (body_ans, (BLetRec (List.combine names new_binds)) :: body_setup)
    | ELambda(args, body, _) ->
       let processBind bind =
         match bind with
         | BName(name, _, _) -> name
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a lambda: %s"
                                             (string_of_bind bind))) in
       (CLambda(List.map processBind args, helpA body, ()), [])
    | ELet((BTuple(binds, _), exp, _)::rest, body, pos) ->
       raise (InternalCompilerError("Tuple bindings should have been desugared away"))
    | EApp(func, args, native, _) ->
       let (func_ans, func_setup) = helpI func in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(func_ans, new_args, native, ()), func_setup @ List.concat new_setup)

    | ESeq(e1, e2, _) ->
       let (e1_ans, e1_setup) = helpC e1 in
       let (e2_ans, e2_setup) = helpC e2 in
       (e2_ans, e1_setup @ [BSeq e1_ans] @ e2_setup)

    | ETuple(args, _) ->
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CTuple(new_args, ()), List.concat new_setup)
    | EGetItem(tup, idx, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (CGetItem(tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem(tup, idx, newval, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (new_imm, new_setup) = helpI newval in
       (CSetItem(tup_imm, idx_imm, new_imm, ()), tup_setup @ idx_setup @ new_setup)
         

    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * unit anf_bind list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])
    | ENil _ -> (ImmNil(), [])

    | ESeq(e1, e2, _) ->
       let (e1_imm, e1_setup) = helpI e1 in
       let (e2_imm, e2_setup) = helpI e2 in
       (e2_imm, e1_setup @ e2_setup)


    | ETuple(args, tag) ->
       let tmp = sprintf "tup_%d" tag in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), (List.concat new_setup) @ [BLet (tmp, CTuple(new_args, ()))])
    | EGetItem(tup, idx, tag) ->
       let tmp = sprintf "get_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (ImmId(tmp, ()), tup_setup @ idx_setup @ [BLet (tmp, CGetItem(tup_imm, idx_imm, ()))])
    | ESetItem(tup, idx, newval, tag) ->
       let tmp = sprintf "set_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (new_imm, new_setup) = helpI newval in
       (ImmId(tmp, ()), tup_setup @ idx_setup @ new_setup @ [BLet (tmp, CSetItem(tup_imm, idx_imm, new_imm,()))])

    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (ImmId(tmp, ()), arg_setup @ [BLet (tmp, CPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [BLet (tmp, CPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (ImmId(tmp, ()), cond_setup @ [BLet (tmp, CIf(cond_imm, helpA _then, helpA _else, ()))])
    | EApp(func, args, native, tag) ->
       let tmp = sprintf "app_%d" tag in
       let (new_func, func_setup) = helpI func in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), func_setup @ (List.concat new_setup) @ [BLet (tmp, CApp(new_func, new_args, native, ()))])
    | ELet([], body, _) -> helpI body
    | ELet((BBlank _, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BSeq exp_ans] @ body_setup)
    | ELetRec(binds, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let processBind (bind, rhs, _) =
         match bind with
         | BName(name, _, _) -> (name, helpC rhs)
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a let-rec: %s"
                                             (string_of_bind bind))) in
       let (names, new_binds_setup) = List.split (List.map processBind binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (ImmId(tmp, ()), (List.concat new_setup)
                        @ [BLetRec (List.combine names new_binds)]
                        @ body_setup
                        @ [BLet(tmp, body_ans)])
    | ELambda(args, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let processBind bind =
         match bind with
         | BName(name, _, _) -> name
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a lambda: %s"
                                             (string_of_bind bind))) in
       (ImmId(tmp, ()), [BLet(tmp, CLambda(List.map processBind args, helpA body, ()))])
    | ELet((BName(bind, _, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELet((BTuple(binds, _), exp, _)::rest, body, pos) ->
       raise (InternalCompilerError("Tuple bindings should have been desugared away"))
  and helpA e : unit aexpr = 
    let (ans, ans_setup) = helpC e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq(exp) -> ASeq(exp, body, ())
        | BLet(name, exp) -> ALet(name, exp, body, ())
        | BLetRec(names) -> ALetRec(names, body, ()))
      ans_setup (ACExpr ans)
  in
  helpP p
;;

let free_vars (e: 'a aexpr) : string list =
  let rec helpA (e: 'a aexpr) =
    match e with
    | ALet (str, cexp, aexp, _) ->  (StringSet.union (helpC cexp) (StringSet.remove str (helpA aexp)))
    | ACExpr cexp -> helpC cexp
    | ASeq (cexp, aexp, _) -> (StringSet.union (helpC cexp) (helpA aexp))
    | ALetRec (binds, aexp, _) ->
      let strs, _ = List.split binds in
      let cexp_set = List.fold_right (fun bind set -> match bind with 
                                                        | (_, cexp) -> StringSet.union (helpC cexp) set) binds StringSet.empty in
      let cexp_set_without_strs = List.fold_right (fun str set -> StringSet.remove str set) strs cexp_set in
      let aexp_set = helpA aexp in
      let aexp_set_without_strs = List.fold_right (fun str set -> StringSet.remove str set) strs aexp_set in
      StringSet.union aexp_set_without_strs cexp_set_without_strs
    and helpC (e: 'a cexpr) = 
    match e with
    | CIf (_, thn, els, _) -> StringSet.union (helpA thn) (helpA els)
    | CLambda (strs, aexp, _) -> let frees = helpA aexp in List.fold_right (fun str set -> StringSet.remove str set) strs frees
    | CPrim1 (_, imm, _) -> helpI imm 
    | CPrim2 (_, imm1, imm2, _) -> StringSet.union (helpI imm1) (helpI imm2)
    | CApp (imm, imm_li, _, _) -> StringSet.union (helpI imm) (List.fold_right (fun imm set -> StringSet.union (helpI imm) set) imm_li StringSet.empty)
    | CImmExpr imm -> helpI imm 
    | CTuple (imm_li, _) -> (List.fold_right (fun imm set -> StringSet.union (helpI imm) set) imm_li StringSet.empty)
    | CGetItem (imm_tup, imm_idx, _) -> StringSet.union (helpI imm_tup) (helpI imm_idx)
    | CSetItem (imm_tup, imm_idx, imm_set, _) -> StringSet.union (helpI imm_set) (StringSet.union (helpI imm_tup) (helpI imm_idx))
    and helpI (e: 'a immexpr) = 
    match e with
    | ImmNum(i, _) -> StringSet.empty
    | ImmBool(b, _) -> StringSet.empty
    | ImmId(x, _) -> StringSet.add x StringSet.empty
    | ImmNil(_) -> StringSet.empty
    in
    StringSet.elements (helpA e)
;;


let ocsh_args = ["$heap"; "$size"];;

(* Implementing "option 2" of creating a variable (3 types of variables: parameters of lambda functions, local bindings,
   and free variables of lambda body) look up environment. 
   Essentially we are building a HASH MAP. 
   
   E.g) Suppose expression 'let x = 1 in letrec f1 = (lambda(a, b): a + b), f2 = (lambda(c): let b1 = 3 in f1(c, b1)) in 1'. 
        We will have 3 stack environments in total (disregard native C functions for now, just think about this conceptually).

     The first stack environment being the environment for the entire expression, and the second stack environment
     for the function (lambda(a): 1). The result should be:

     all stack envs = [(env of whole expression: [x: [RBP -8], f1: [RBP - 16], f2: [RBP - 24]]), 
                       (env of f1: [a: [RBP + 24], b: [RBP + 32]]),
                       (env or f2: [c: [RBP + 24], f1: [RBP - 8], b1: [RBP - 16]])]

    *Note* Remember that for any stack frame of function f, [RBP + 0] is saved RBP, [RBP + 8] is return address, [RBP + 16] 
       is closure of f, [RBP + 24] and onwards are the regular arguments of f.) 
*)

let naive_stack_allocation (prog: tag aprogram) : tag aprogram * arg name_envt tag_envt =
  (* Top-level function of extending our environment which is a hash map, by providing the (tag, value) pair, where 'tag' is the
     key to check if its in the hash map. If the tag key is in the hashmap, we extend the list which is the value of this key by 
     the 'value'. Else we create a new hashmap element with this 'tag' as the key. *)
  let add_to_tag_env (target_tag : tag) (env : arg name_envt tag_envt) (value : (string * arg)) =
    if List.mem_assoc target_tag env then
      List.fold_right (fun (tag, name_env) so_far -> if target_tag = tag then (tag, value::name_env)::so_far else (tag, name_env)::so_far)
      env []
    else (target_tag, value::[])::env
  in
  (* Helper function that extends a provided environment which again is a hash map, given the following three things:
     1) si (stack index): Equal to the # of stack objects that are negatively offset from RBP. 
     2) target_tag: Tag of the expression which denotes a unique stack environment. Each lambda expression will have a unique
     tag, and the entire expression (which is really the function our_code_starts_here) will also have a unique tag.
     3) e: This is the expression that we will extract stack environments from. 
  *)
  let rec helpAExpr (si : int) (target_tag : tag) (env : arg name_envt tag_envt) (e : tag aexpr) : arg name_envt tag_envt =
    match e with
    (* Map the let bind name to negative offset of RBP, and then add it to hashmap. Then expand the hashmap by the bound
       expression of the let bind. Then expand the hashmap by the body expression of the let form. *)
    | ALet (str, cexp, aexp, _) ->
        let new_arg = (str, RegOffset (~-8 * si, RBP)) in
        let new_env = add_to_tag_env target_tag env new_arg in
        let after_cexpr = helpCExpr (si + 1) target_tag new_env cexp in
        let after_aexp = helpAExpr (si + 1) target_tag after_cexpr aexp in
        after_aexp
    (* Expand the hashmap by cexp (i.e. 'operation anf'). *)
    | ACExpr cexp -> helpCExpr si target_tag env cexp
    (* Expand the hashmap by cexp which is the first expression, and then again by aexp which is the second expression.*)
    | ASeq (cexp, aexp, _) -> 
      let after_cexpr = helpCExpr si target_tag env cexp in
      let after_aexpr = helpAExpr si target_tag after_cexpr aexp in
      after_aexpr
    (* Same idea as ALet, but while ALet is a nested structure with one binding in a particular scope of let, ALetRec has multiple
       bindings in the same scope. *)
    | ALetRec (binds, body, _) ->
      let new_args = List.mapi (fun i bind -> match bind with
      | (str, _) -> (str, RegOffset(~-8 * (si + i), RBP))) binds in 
      let after_arg_names = List.fold_right (fun value so_far -> add_to_tag_env target_tag so_far value) new_args env in
      let after_cexprs = List.fold_right (fun (_, cexpr) so_far -> helpCExpr (si + List.length binds) target_tag so_far cexpr) binds after_arg_names in
      let after_body = helpAExpr (si + List.length binds) target_tag after_cexprs body in
      after_body
  (* Helper function that extends a provided environment, for compound expressions (i.e. I call them 'operation anf expressions')
     . Since 'If' expressions and 'lambda' expressions are the only kinds of 'op anf' that can have bindings, they have to
     be handled differently. The extension of hashmap by rest of 'op anf', simply returns the original hashmap. *)
  and helpCExpr (si : int) (target_tag : tag) (env : arg name_envt tag_envt) (e : tag cexpr) : arg name_envt tag_envt =
    match e with
    (* Extend the current hashmap by applying helpAExpr on thn_aexpr with the current hashmap, current tag, and current si. 
       Then extend the resulting hashmap by applying helpAexpr on the els_aexpr with the above result hashmap, current tag,
       and current si. *)
    | CIf (_, thn_aexp, els_aexp, _) -> 
      let after_then = helpAExpr si target_tag env thn_aexp in
      let after_else = helpAExpr si target_tag after_then els_aexp in
      after_else
    (* The stack env of a lambda expression is lambda expression's tag -> stack environment mapping.
       This stack env should be extending the current env, but it will be done so by inserting each (tag, mapping value) pair to
       the hash map using the add_to_tag_env function. 
       I.e. Create a new map consisting of the regular arguments of the lambda to positive offsets, free variables of the
       lambda to the negative offsets, and local variables of the lambda to the negative offsets. Note that we don't add
       the closure of the lambda as a stack env mapping, since it is not technically not a binding, even though it is an
       argument and a part of the stack environment. 
     *)
    | CLambda(binds, body, t) ->
      let new_env = (t, [])::env in
      let new_binds = List.mapi (fun i str -> (str, RegOffset(24 + (8 * i), RBP))) binds in
      let after_binds = List.fold_right (fun value so_far -> add_to_tag_env t so_far value) new_binds new_env in
      let after_body = helpAExpr (1 + (List.length (free_vars (ACExpr(e))))) t after_binds body in
      let frees = List.sort String.compare (free_vars (ACExpr(e))) in
      let free_env = List.mapi (fun i str -> (str, RegOffset(~-8*(i+1), RBP))) frees in
      let after_free_env = List.fold_right (fun value so_far -> add_to_tag_env t so_far value) free_env after_body in
      after_free_env
    | _ -> env
  in
  (* The stack environment of AProgram(body, t) is constructed as follows:
      1) Sub-environment correponding to the mappings of the native C functions - 'print', 'equal', 'input':
            Map each var to labels which is "?" ^ var, e.g. [(print, "?print"), (equal, "?equal"), (input, "?input")] 
            and then assign it to tage of the AProgram, such that starter_tag_env = 
           [(t, [(print, "?print"), (equal, "?equal"), (input, "?input")])]

      2) All sub-environments of the program body:
            Extend the above sub-environment (1) using helpAExpr function. 

      3) Create mappings of the arguments of our_code_starts_here(uint64_t* HEAP, uint64_t size) C function which is again
      the very first function called in our entire program:
            ocsh_args = ["$heap"; "$size"] => ocsh_binds = [("$heap", RBP + 32), ("$size", RBP + 40)]
      And then insert these mappings into (2).  
  *)
  match prog with
  | AProgram (body, t) ->
      let starter_name_env = List.map (fun (str, _) -> (str, Label("?" ^ str))) initial_val_env in
      let starter_tag_env = [(t, starter_name_env)] in
      let body_env = helpAExpr 1 t starter_tag_env body in
      let ocsh_binds = List.mapi (fun i str -> (str, RegOffset(24 + (8 * i), RBP))) ocsh_args in
      let extended_env = List.fold_right (fun bind so_far -> (add_to_tag_env t so_far bind)) ocsh_binds body_env in 
      (prog, extended_env)
;;

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq(e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ALetRec(binds, body, _) ->
       (List.length binds) + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf(_, t, f, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in helpA e

(* All are using RAX right now, just in case any changes need to be made *)
let bool_op_prologue reg reg_arg =
  [ IMov (Reg reg, reg_arg);
    IAnd (Reg reg, HexConst bool_tag_mask);
    ICmp (Reg reg, HexConst bool_tag);
    IMov (Reg reg, reg_arg) ]
;;

let tuple_op_prologue reg reg_arg =
  [ IMov (Reg reg, reg_arg);
    IAnd (Reg reg, HexConst bool_tag_mask);
    ICmp (Reg reg, HexConst tuple_tag);
    IMov (Reg reg, reg_arg) ]
;;

let number_op_prologue reg reg_arg =
  [ IMov (Reg reg, reg_arg);
    IAnd (Reg reg, HexConst num_tag_mask);
    ICmp (Reg reg, HexConst num_tag);
    IMov (Reg reg, reg_arg) ]
;;

let rec replicate x i =
  if i = 0 then []
  else x :: (replicate x (i - 1))
  
(* ********Note**********
   So this is the key function that performs garbage collection whenever we allocate new memory onto the heap.
   This process always occurs right after we allocate memory on the heap (i.e. allocating tuple or closure on the heap), 
   where we check if we actually have enough heap space to allocate this closure.

   This is the process: Check if there is enough space in the heap to allocate n words. If there is enough space,
   then skip garbage collection. Else if there is not enough space, then perform garbage collection.
   
    1) Create jump label, call it 'ok'/"$memcheck_tag#", that allows skipping garbage collection.
    2) Place the code pointer of HEAP_END, i.e. code ptr which is '?HEAP_END', into scratch register.
    3) Dereference the scratch register which is pretty much dereferencing the code_ptr '?HEAP_END'. 
    The value of code ptr '?HEAP_END' is HEAP_END so dereferencing '?HEAP_END' returns HEAP_END which is the heap address.
    4) Subtract from scratch register the allocated space for n words on the heap, and put this into RAX.
    5) Then compare RAX with heap register (R15). This is comparing HEAP_END with the current heap pointer.
    6) If above test passes, i.e. HEAP_END - heap ptr >= n words * 8 bytes, then jump to 'ok' label to skip
    garbage collection.
   
    I.e. All we are doing is trying to see if 'HEAP_END - Heap pointer >= space we allocated for n words on the heap'
      is true or not. If this is true, then we have enough space and so we don't need to garbage collect. 
      If this is not true, then we don't have enough space and so we do need to garbage collect.

    7) If the above test fails, i.e. there is not enough memory to allocate n words, then perform garbage collection.
    8) After successfully garbage collecting, return the new heap pointer, one that should now be smaller than the heap pointer
    we started with, and now satisfies 'HEAP_END - new heap pointer >= space we allocated for n words on the heap'. 

    We are done. 
   *)
and reserve size tag =
  let ok = sprintf "$memcheck_%d" tag in
  [
    IInstrComment(IMov(Reg(scratch_reg), Label("?HEAP_END")),
                 sprintf "Reserving %d words" (size / word_size));
    IMov(Reg RAX, RegOffset(0, scratch_reg));
    ISub(Reg(RAX), Const(Int64.of_int size));
    ICmp(Reg(RAX), Reg(heap_reg));
    IJge(Label ok);
  ]
  @ (native_call (Label "?try_gc") [
         (Sized(QWORD_PTR, Reg(heap_reg))); (* alloc_ptr in C *)
         (Sized(QWORD_PTR, Const(Int64.of_int size))); (* bytes_needed in C *)
         (Sized(QWORD_PTR, Reg(RBP))); (* first_frame in C *)
         (Sized(QWORD_PTR, Reg(RSP))); (* stack_top in C *)
    ])
  @ [
      IInstrComment(IMov(Reg(heap_reg), Reg(RAX)), "assume gc success if returning here, so RAX holds the new heap_reg value");
      ILabel(ok);
    ]


(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT *)
(* Additionally, you are provided an initial environment of values that you may want to
   assume should take up the first few stack slots.  See the compiliation of Programs
   below for one way to use this ability... *)

(* compile_fun compiles the function call of our program, i.e. it compiles the function call
   our_code_starts_here(heap_ptr_val, size_val), where the body of our_code_starts_here is the whole program that we are compiling. 
   As with compilation of our lambda, the compilation of ocsh will consist of three parts: (1) name of function which is ocsh,
   (2) the body of function which is our program, (3) the end of our function (this will be empty since we don't create a closure
   for ocsh)
   
   ***Note*** that unlike lambdas which exist as objects as closures in the heap, ocsh will not exist as closures in the heap. It is
   unnecessary to construct the closure of ocsh in the heap, because ocsh is itself never called by another lambda, and the whole
   purpose of closure is that it is the first (implicit) argument of a function call.
   Note that ocsh still has free variables, and unlike arbitrary number of free variables, the number of free vars ocsh has
   is constant - 3 free variables for the three native C functions '?print', '?equal', and '?input'. These free variables, are
   accessed directly via their code-ptr/label (i.e. "?print", "?equal", "?input") rather than being accessed as negative offsets
   of RBP which is the way that lambda accesses its free variables. 

 *)
and compile_fun name tag args body initial_env =
  (* Get all the free variables in this expression. *)
  (* Only three free variables in the body: '?print', '?equal', '?input' *)
  let free = free_vars body in 
  (* From these three variables set subtract '$heap' and '$size', which are the two parameters of ocsh that we provide in main.c *)
  let free = List.fold_right (fun str so_far -> StringSet.remove str so_far) args (StringSet.of_list free) in
  let free = List.sort String.compare (StringSet.elements free) in
  (* Allocate stack slots (i.e. negative offsets from RBP) equal to #free vars + #local bindings in body = 3 + #locals *)
  let rec push_zeros amt = if amt = 0 then [] else (IPush(Const(0L)))::(push_zeros (amt-1)) in 
  let lambda_env =
   (List.assoc tag initial_env)
in 
  let arg_len = List.length args in
  let reserveSpace =
     (push_zeros (List.length free + (deepest_stack body lambda_env)))
    in 
  (* This is the entire set of instructions for the ocsh. *)
  ( [
    ILabel(name); 
    ILineComment("prologue"); 
    IPush(Reg RBP); 
    IMov(Reg RBP, Reg RSP)
    ] 
    @ reserveSpace
    , 
    [ILabel(name ^ "_body")] @
    compile_aexpr body initial_env tag arg_len true
    ,
    [IMov(Reg RSP, Reg RBP); IPop(Reg RBP); IRet; ILabel(name ^ "_end")]
  )

(* Helper function for moving arguments into the first 6 registers of a function call, for x64 calling convention,
   which is used for native C function calls where number of arguments is <= 6. *)
and args_help args regs =
  match args, regs with
  | arg :: args, reg :: regs ->
    IMov(Sized(QWORD_PTR, Reg(reg)), arg) :: args_help args regs
  | args, [] ->
    List.rev_map (fun arg -> IPush arg) args
  | [], _ -> []

(* Assembly instructions for calling a native C function. If number of arguments of native C function is less than 6, simply put 
   the arguments into the 6 argument registers. Else if number of arguments is greater than 6, we need to use the regular calling
  convention of pushing arguments as positive offsets of RBP, while making sure that the RBP is 16-byte aligned by adding
   0 ~ 1 padding. 
   
   *Note* In the implementation of our compiler so far, we have no native C function that uses more than 6 arguments, so we won't
   ever push the arguments of C functions onto stack and deal with 16-byte stack alignment. *)
and native_call label args =
  (* We know that on entry to every function, RSP is 16-byte aligned.
     We know that every frame is a multiple of 16 bytes.
     The call instruction pushes one return pointer onto the stack.
     The first thing we do is push RBP onto the stack
     So, we add 8 bytes of padding IFF the number of spilled args is *ODD*.
  *)
  let num_stack_args = max (List.length args - 6) 0 in
  let padding_needed = (num_stack_args mod 2) <> 0 in
  let setup = (if padding_needed
               then [IInstrComment(IPush(Sized(QWORD_PTR, Const(0L))), "Padding to 16-byte alignment")]
               else []) @ args_help args first_six_args_registers in
  (* After returning from function call, we pop the arguments from the stack if we are using stack to push arguments if
     (again, this is the case when there are more than 6 arguments for the native C function being called.) *)
  let teardown =
    (if num_stack_args = 0 then []
     else [ IInstrComment(IAdd(Reg(RSP), Const(Int64.of_int(word_size * num_stack_args))),
                          sprintf "Popping %d arguments" num_stack_args) ])
    @ (if padding_needed then [IInstrComment(IAdd(Reg(RSP), Const(Int64.of_int word_size)), "Unpadding one word")]
       else []) in
  setup @ [ ICall(label) ] @ teardown

(* Compilation process for aexpr. 
   e: expression to compile
   env: hashmap env which is the global environment that contains all stack envs. 
   tag_num: tag of the function whose body contains this expression e. 
   num_args: the number of arguments of the function call whose body expression is the superset of the current expression
             being compiled. 
   is_tail: true if the current sub-expression is in tail position, else false. 
    
  **Note**: 
  While it is implicit, the starting function call is the call of ocsh (our_code_starts_here), such that the
  starting stack env belongs to ocsh.  
  *)
and compile_aexpr (e : tag aexpr) (env : arg name_envt tag_envt) (tag_num : tag) (num_args : int) (is_tail : bool) :
    instruction list =
  match e with
  (* 1) Compile bound expression and put into RAX
     2) Then, Move RAX into the stack loc corresponding to bind name. Find this stack loc by using tag_num as key of
     hashmap env to first find the stack env that this ALet belongs to, then use 'find' on this stack env for the bind name.
     3) Then, compile body. 

     *Note* Semantically, if the bound expression of ALet is a lambda, then the free variables should only be variables that are 
     bound to the outside/previously-defined bindings. 
     And since the lambda of ALet binding is never a member of mutually recursive set of bindings, i.e. it can only have as its
     free variable a lambda that was defined before it in which case no patching is necessary because the construction of our closure
     will place the actual heap address of the previously defined lambda free var in it, and never the value 0 which is the initial
     value that is assigned to any variable on the stack frame when the stack frame is created.
     Furthermore it should never have a lambda that is defined after it as its free variable and so to do patching would be 
     semantically incorrect, as this is the meaning of let rec.  
     (More on the concept of "patching" which I fleshed out below for ALetRec compilation.)
  *)
  | ALet (x, exp, body, _) ->
      let prelude = compile_cexpr exp env tag_num num_args false in
      let body = compile_aexpr body env tag_num num_args is_tail in
      let closure_env = List.assoc tag_num env in
      prelude @ [IMov (find closure_env x, Reg RAX)] @ body
  (* Compile the 'op anf' with the hashmap env. *)
  | ACExpr cexp -> compile_cexpr cexp env tag_num num_args is_tail
  (* Compile the first expression, and then the second expression. *)
  | ASeq (cexp, aexp, _) -> 
    (compile_cexpr cexp env tag_num num_args false) @
    (compile_aexpr aexp env tag_num num_args true)
  (* compile_any_anf(let rec b1 = lambda_e1, ..., bn = lambda_en in any_anf)
     1) For each recursive bindings, compile the bound expression which is a lambda into a closure
     put the closure in the stack location of the binding name
     2) Patching free variables of let rec lambda closures: Given 'let rec b1 = lamb1, b2 = lamb2, ..., bn = lambn in body',
     after the construction of closure for lambn, for each free variable in each closure of lamb1 ~ lambn, "patch" it.  
     3) Compile the body. 
     
     Do 1) -> 2) -> 3) and we are done. 

     [****Note****: Question? If we already construct the closure for each let rec lambda when we compile CLambda, such that the
     free variables are looked up from closure_env and then put in the closure appropriately, why do we have to patch them again
     after the construction of the last lambda in the let rec expression?

     Answer) In the closure_env, the initial values of these closures are 0, because we initially create the
     stack frame of closure_env by "pushing zeroes" to allocate space for these closures. It is during the construction of 
     each closure that we put the heap location into the stack location of closure_env, replacing the value 0 with actual
     heap location of the closure. 
     This means that when we construct the ith closure during compilation of CLambda of that ith closure, if one of its 
     free variable is the bind name of jth closure in let rec such that i < j, the value of [RBP - j * 8] will be put in the 
     ith closure, but at this point [RBP - j * 8] contains 0. 

     I.e, without "patching", for any two mutually recursive lambda_i and lambda_j such that i < j and the bind name of 
     lambda_j is a free var in lambda_i, the construction of closure of lambda_i will have the default value 0 
     where the heap address for closure of lambda_j should be, BECAUSE AT THIS POINT THE CLOSURE FOR lambda_j HAS NOT YET BEEN
     CREATED ON THE HEAP. 

     Therefore after constructing all the closures for all mutually recursive lambdas, we have to perform a patching process,
     such that for each closure of mutually recursive lambda, for each free variable that is a bind name of a mutually recursive 
     lambda, the temp value of 0 in the closure is replaced with the heap address of the closure of the mutually recursive lambda.
     
     E.g) let dummy = 100 in let rec f1 = (lambda(x): f2(x)), f2 = (lambda(y): f3(y)), f3 = (lambda(z): f1(z))  in 1.
     0) closure_env: [dummy: [RBP - 8], f1: [RBP - 16], f2: [RBP - 24], f3: [RBP - 32]]
        Current stack frame of closure_env: [dummy: 100, f1: 0, f2: 0, f3: 0]

     1) Constructing closure for f1: 
        Current stack frame of closure_env: [dummy: 100, f1: 0, f2: 0, f3: 0]
        Make f1 closure = (arity: 1, codePtr: lambda_tag#i, #freevars: 1, codeBodyPtr: lambda_body_tag#i, f2: [RBP-24] = 0, padding)
        Current stack frame of closure_env: [dummy: 100, f1: [R15 + 48], f2: 0, f3: 0]

     2) Constructing closure for f2:
        Current stack frame of closure_env: [dummy: 100, f1: [R15 + 48], f2: 0, f3: 0]
        Make f2 closure = (arity: 1, codePtr: lambda_tag#j,  #freevars: 1, codeBodyPtr: lambda_body_tag#j, f3: [RBP-32] = 0, padding)
        Current stack frame of closure_env: [dummy: 100, f1: init heap ptr + 48, f2: init heap ptr + 96, f3: 0]
     3) Constructing closure for f3:
        Current stack frame of closure_env: [dummy: 100, f1: init heap ptr + 48, f2: init heap ptr + 96, f3: 0]
        Make f3 closure = (arity: 1, codePtr: lambda_tag#j, #freevars: 1, codeBodyPtr: lambda_body_tag#j, 
                           f1: [RBP-16] = init heap ptr + 48, padding)
        Current stack frame of closure_env: [dummy: 100, f1: init heap ptr + 48, f2: init heap ptr + 96, f3: init heap ptr + 144]

     At this point the stack frame of closure_env properly contains all closures. 
     Current stack frame of closure_env: [dummy: 100, f1: init heap ptr + 48, f2: init heap ptr + 96, f3: init heap ptr + 144]
     
     And except for the last closure, i.e. closure f3, all other closures have free variable of let rec bind that have value of 0,
     instead of the closure of the lambda of that bind.  
     Current f1 closure: (arity: 1, codePtr: lambda_tag#i, #freevars: 1, codeBodyPtr: lambda_body_tag#i, f2: [RBP-24] = 0, padding)
     Current f2 closure: (arity: 1, codePtr: lambda_tag#j, #freevars: 1, codeBodyPtr: lambda_body_tag#j, f3: [RBP-32] = 0, padding)

     So we have to patch these two closures. 
     And now you can see why patching closures has to be done after the construction of all the closures of the let rec lambdas.
     Technically, the only free vars of a lambda that needs to be patched are bind names of its mutually recursive lambdas,
     but for simplicity sake of implementation we will patch non-lambda free-vars which is placing the value of a regular 
     free-var in its stack slot in closure-env which is already there. 
  *)
  | ALetRec (binds, body, _) -> 
    (* Get the stack env that contains all binds of ALetRec *)
    let closure_env = List.assoc tag_num env in
    (* Define a helper that gets stack env of a lambda expression. *)
    let get_lambda_env cexpr = match cexpr with
      | CLambda(_, _, t) -> List.assoc t env
      | _ -> raise (NotYetImplemented "Right side of letrec is not lambda") in
    (* For each let rec binding, compile the lambda and then place the lambda closure into its stack slot in the closure_env. *)
    let prelude = List.fold_right (fun (bind, exp) so_far -> 
      (compile_cexpr exp env tag_num num_args true) @ [IMov (find closure_env bind, Reg RAX)] @ so_far) binds [] in
    (* Patch all free vars of each lambda for all lambdas of ALetRec as the last step. 
       For each lambda: for each free var in this lambda, find its stack slot in closure_env and put it to RAX, 
       and then put RAX into the free var heap slot in the closure of lambda. *)
    let patch_vars = List.concat (List.map (fun (lambda_name, exp) -> 
      let lambda_free_vars = List.sort String.compare (free_vars (ACExpr(exp))) in 
      let lambda_env = get_lambda_env exp in (* Not actually using lambda_env for anything other than error checking. *)
      List.concat (List.mapi (fun i free_var -> 
        try let _ = List.assoc free_var binds in 
        [IMov(Reg RAX, find closure_env free_var); IMov(Reg scratch_reg, find closure_env lambda_name);
          ISub(Reg scratch_reg, HexConst closure_tag);
          IMov(RegOffset(32 + (i * 8), scratch_reg), Reg RAX)]
      with Not_found -> []) lambda_free_vars
      )) binds) 
     in
     prelude @ 
     [ILineComment("Starting Patching for mutually recursive functions")] @ patch_vars @
     compile_aexpr body env tag_num num_args is_tail 

(* Same idea as fer-de-lance, except I'm using hashmap env. 
   1) Use 'List.assoc tag_num env' to get stack env of current function whose call we are in, 
      where tag_num is the tag of the function. 
   2) Using the stack env from 1), find the stack location of each variable. *)
and compile_cexpr (e : tag cexpr) (env : arg name_envt tag_envt) (tag_num : tag) (num_args : int) (is_tail : bool) :
    instruction list =
  match e with
  | CIf (cond, thn, els, tag) ->
      let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
      let imm_env = List.assoc tag_num env in
      [ IMov (Reg RAX, compile_imm cond imm_env);
        IAnd (Reg RAX, HexConst bool_tag_mask);
        IMov (Reg scratch_reg, HexConst bool_tag);
        ICmp (Reg RAX, Reg scratch_reg);
        IJne (Label "?err_if_not_bool");
        IMov (Reg RAX, compile_imm cond imm_env);
        IMov (Reg scratch_reg, const_false);
        ICmp (Reg RAX, Reg scratch_reg);
        IJe (Label else_label) ]
      @ compile_aexpr thn env tag_num num_args is_tail
      @ [IJmp (Label done_label); ILabel else_label]
      @ compile_aexpr els env tag_num num_args is_tail
      @ [ILabel done_label]
  (* Don't get confused!!! 
     Easy way to remember is that every function/lambda has a stack env. 
  
     Since CApp (i.e. function call) is a subexpression of some function body, the stack env that CApp looks-up from is 
     the stack env of the function whose body contains this CApp. And CApp only looks up one thing from this stack env, the 
     name of the function of CApp itself. 

     (And the stack env to use for compiling CLambda is the stack env of the lambda represented by CLambda.)
     
     I.e. Compiling the call of function f1 uses the stack env of the outer function f2 whose body calls f1. 
          Compiling a function f1 uses the stack env of f1. 
          
    After we access the lambda closure, we will then do the actual call of the lambda definition which in its default form
    creates a new stack frame and then executes the body. The label that creates the new stack frame happens before the label
    of the lambda body, and the stack frame creating label is the 2nd element and therefore stored as [heap address of closure + 8].  
    If this function call is a tail call, we skip the stack frame creation and just call the label of lambda body which is stored
    in the closure as 4th element and therefore stored as [heap address of closure + 16]. 
    (remember closure: (arity, code_ptr (stack frame creation label), # free vars, code_body_ptr (lambda body label), free var 1,
                        free var 2, ..., free var n, padding))
    
     E.g) 'let f = (lambda(x): let y = 5 in x) in f(2)', here this is really us calling the following function:
          'ocsh(heap, size): let f = (lambda(x): let y = 5 in x + y) in f(2)'
          The stack env of ocsh is [f: [RBP - 8]], and stack env of f is [x: [RBP + 24], y: [RBP - 8]]
          The function call (or rather lambda call) 'f(2)' first looks up 'f' from [f: [RBP - 8]]
          Then we now that we have the closure of 'f', we call the code ptr, either the default code ptr or body code ptr, 
          depending on whether we have a valid tail call or not, in this closure. 
  *)
  | CApp (funname, args, call_t, _) -> (
    let imm_env = List.assoc tag_num env in (* First get the stack env of the function that this fun call belongs to. *)
    let fun_arg = compile_imm funname imm_env in (* Get the closure of function. *)
    if call_t = Native then (* If call is native to C, then do the following:*) 
      let push_args_instr = (* Instructions for pushing the arguments in the 6 argument registers. Else if more than 6 arguments,
                               push them on the stack. *)
        List.concat
          (List.rev
             (List.mapi
                (fun i imm ->
                  if i < 6 then
                    [IMov (Reg (List.nth first_six_args_registers i), compile_imm imm imm_env)]
                  else
                    [IMov (Reg R10, compile_imm imm imm_env); IPush (Reg R10)] )
                args ) )
      in
      let pop_args_instr = (* Instructions for popping arguments. Do nothing if arguments were in argument registers. 
                              Else if more than 6 arguments, pop them from the stack. *)
        List.concat
          (List.mapi
             (fun i _ ->
               if i < 6 then
                 []
               else
                 [IPop (Reg R10)] )
             args )
      in
      push_args_instr @ [ICall fun_arg] @ pop_args_instr (* Push args, call the function, then pop the args. *)
    else (* Else, if the call is not native to C, do the following: *)
      let new_args = [funname] @ (* Given that the function is not native to C, we will have closure as the first argument,
                                    so the set of arguments is closure + regular arguments. *)
        if List.length args mod 2 = 0 then
          args @ [ImmNum (0L, 0)]
        else
          args
      in
      let push_args_instr = (* Instructions for pushing arguments onto the stack: For each argument, access their location
                               in the stack env of the function call, and then push these locations onto stack frame. *)
        List.concat
          (List.rev
             (List.mapi (fun _ imm -> [IMov (Reg scratch_reg, compile_imm imm imm_env); IPush (Reg scratch_reg)]) new_args) )
      in
      let pop_args_instr = List.map (fun _ -> IPop (Reg scratch_reg)) new_args in (* Instructions for popping arguments *)
      let call_instr = [ (* Instructions for setting up the call. 'call_instr' is a bit of a misnomer. What we do here
                            is loading the code ptr to lambda onto RAX and some type checking. *)
        ILineComment("Start call of lambda");
        IMov(Reg RAX, fun_arg); 
        IMov(Reg scratch_reg, Reg RAX);
        IMov(Reg RAX, Reg scratch_reg);
        IAnd(Reg RAX, HexConst closure_tag_mask);
        ICmp(Reg RAX, HexConst closure_tag);
        IJne(Label("?err_call_not_closure"));
        ISub(Reg scratch_reg, HexConst closure_tag); 
        IMov(Reg RAX, RegOffset(0, scratch_reg));
        ICmp(Reg RAX, Const(Int64.of_int((List.length args) * 2)));
        IJne(Label("?err_call_arity_err"));
        IMov(Reg RAX, RegOffset(8, scratch_reg)); (* The code pointer/ label for creating stack frame for a 
                                                     lambda is at [heap location + 8]*)
      ] in
      let push_swap_instr =
        List.concat
          (List.rev
             (List.mapi (fun _ arg -> [IMov (Reg RAX, compile_imm arg imm_env); IPush (Reg RAX)]) args) )
      in
      let pop_swap_instr =
        List.concat
          (List.mapi
             (fun i _ -> [IPop (Reg RAX); IMov (RegOffset (24 + (i * 8), RBP), Reg RAX)])
             args )
      in
      if true && is_tail && List.length args <= num_args then (* If the call is a tail-call and satisfies the condition
        that the callee has leq arguments than caller, then we use the current stack frame: 1) put the closure into 
        [RBP + 16], 2) move the label for lambda body which is at [heap address of closure + 24] into RAX.  *)
        push_swap_instr @ pop_swap_instr @ call_instr @ 
        [IAdd(Reg scratch_reg, HexConst closure_tag); IMov(RegOffset(16, RBP), Reg scratch_reg); 
         ISub(Reg scratch_reg, HexConst closure_tag); IMov(Reg RAX, RegOffset(24, scratch_reg)); IJmp(Reg RAX)]
      else (* Else if the call is not a tail-call, then simply do the set-up for call instruction (i.e. call_instr), 
              then push args, call the label for stack frame creation of lambda, then pop args. *)
        call_instr @ 
        [ILineComment("Pushing args on stack for call of lambda")] @
        push_args_instr @ 
        [ICall (Reg RAX);] @
        pop_args_instr )
  | CImmExpr e -> 
    let imm_env = List.assoc tag_num env in
    [IMov (Reg RAX, compile_imm e imm_env)]
  | CPrim1 (Add1, arg, _) ->
      let imm_env = List.assoc tag_num env in
      number_op_prologue RAX (compile_imm arg imm_env)
      @ [IJne (Label "?err_arith_not_num"); IAdd (Reg RAX, Const 2L); IJo (Label "?err_overflow")]
  | CPrim1 (Sub1, arg, _) ->
      let imm_env = List.assoc tag_num env in
      number_op_prologue RAX (compile_imm arg imm_env)
      @ [IJne (Label "?err_arith_not_num"); ISub (Reg RAX, Const 2L); IJo (Label "?err_overflow")]
  | CPrim1 (Print, arg, _) ->
      let imm_env = List.assoc tag_num env in
      [IMov (Reg RAX, compile_imm arg imm_env); IMov (Reg RDI, Reg RAX); ICall (Label "?print")]
  | CPrim1 (IsBool, arg, tag) ->
      let label_tag = sprintf "is_bool_true#%d" tag in
      let imm_env = List.assoc tag_num env in
      bool_op_prologue RAX (compile_imm arg imm_env)
      @ [IMov (Reg RAX, const_true); IJe (Label label_tag); IMov (Reg RAX, const_false); ILabel label_tag]
  | CPrim1 (IsNum, arg, tag) ->
      let label_tag = sprintf "is_num_true#%d" tag in
      let imm_env = List.assoc tag_num env in
      number_op_prologue RAX (compile_imm arg imm_env)
      @ [IMov (Reg RAX, const_true); IJe (Label label_tag); IMov (Reg RAX, const_false); ILabel label_tag]
  | CPrim1 (IsTuple, arg, tag) ->
      let label_tag = sprintf "is_tuple_true#%d" tag in
      let imm_env = List.assoc tag_num env in
      tuple_op_prologue RAX (compile_imm arg imm_env)
      @ [IMov (Reg RAX, const_true); IJe (Label label_tag); IMov (Reg RAX, const_false); ILabel label_tag]
  | CPrim1 (Not, arg, _) ->
      let imm_env = List.assoc tag_num env in
      bool_op_prologue RAX (compile_imm arg imm_env)
      @ [ IJne (Label "?err_logic_not_bool");
          IMov (Reg scratch_reg, bool_mask);
          IXor (Reg RAX, Reg scratch_reg) ]
  | CPrim1 (PrintStack, arg, _) ->
      let imm_env = List.assoc tag_num env in
      [ 
        IMov (Reg RAX, compile_imm arg imm_env);
        IMov (Reg RDI, Reg RAX);
        IMov (Reg RSI, Reg RSP);
        IMov (Reg RDX, Reg RBP);
        IMov (Reg (List.nth first_six_args_registers 3), Const (Int64.of_int num_args));
        ICall (Label "?print_stack") ]
  | CPrim2 (Plus, l, r, _) ->
      let imm_env = List.assoc tag_num env in
      let r_arg = compile_imm r imm_env in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_arith_not_num")]
      @ number_op_prologue RAX (compile_imm l imm_env)
      @ [ IJne (Label "?err_arith_not_num");
          IMov (Reg scratch_reg, r_arg);
          IAdd (Reg RAX, Reg scratch_reg);
          IJo (Label "?err_overflow") ]
  | CPrim2 (Minus, l, r, _) ->
      let imm_env = List.assoc tag_num env in
      let r_arg = compile_imm r imm_env in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_arith_not_num")]
      @ number_op_prologue RAX (compile_imm l imm_env)
      @ [ IJne (Label "?err_arith_not_num");
          IMov (Reg scratch_reg, r_arg);
          ISub (Reg RAX, Reg scratch_reg);
          IJo (Label "?err_overflow") ]
  | CPrim2 (Times, l, r, _) ->
      let imm_env = List.assoc tag_num env in
      let r_arg = compile_imm r imm_env in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_arith_not_num")]
      @ number_op_prologue RAX (compile_imm l imm_env)
      @ [ IJne (Label "?err_arith_not_num");
          IMov (Reg scratch_reg, r_arg);
          ISar (Reg RAX, Const 1L);
          IMul (Reg RAX, Reg scratch_reg);
          IJo (Label "?err_overflow") ]
  | CPrim2 (And, l, r, _) ->
      let imm_env = List.assoc tag_num env in
      let r_arg = compile_imm r imm_env in
      bool_op_prologue RAX r_arg @ [IJne (Label "?err_logic_not_bool")]
      @ bool_op_prologue RAX (compile_imm l imm_env)
      @ [IJne (Label "?err_logic_not_bool"); IMov (Reg scratch_reg, r_arg); IAnd (Reg RAX, Reg scratch_reg)]
  | CPrim2 (Or, l, r, _) ->
      let imm_env = List.assoc tag_num env in
      let r_arg = compile_imm r imm_env in
      bool_op_prologue RAX r_arg @ [IJne (Label "?err_logic_not_bool")]
      @ bool_op_prologue RAX (compile_imm l imm_env)
      @ [IJne (Label "?err_logic_not_bool"); IMov (Reg scratch_reg, r_arg); IOr (Reg RAX, Reg scratch_reg)]
  | CPrim2 (Greater, l, r, tag) ->
      let imm_env = List.assoc tag_num env in
      let jump_label = sprintf "greater#%d" tag in
      let r_arg = compile_imm r imm_env in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_comp_not_num")]
      @ number_op_prologue RAX (compile_imm l imm_env)
      @ [ IJne (Label "?err_comp_not_num");
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJg (Label jump_label);
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (GreaterEq, l, r, tag) ->
      let imm_env = List.assoc tag_num env in
      let r_arg = compile_imm r imm_env in
      let jump_label = sprintf "greater_eq#%d" tag in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_comp_not_num")]
      @ number_op_prologue RAX (compile_imm l imm_env)
      @ [ IJne (Label "?err_comp_not_num");
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJge (Label jump_label);
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (Less, arg1, arg2, tag) ->
      let imm_env = List.assoc tag_num env in
      let l_arg = compile_imm arg1 imm_env in
      let r_arg = compile_imm arg2 imm_env in
      let jump_label = sprintf "less#%d" tag in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_comp_not_num")] @ number_op_prologue RAX l_arg
      @ [ IJne (Label "?err_comp_not_num");
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJl (Label jump_label);
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (LessEq, arg1, arg2, tag) ->
      let imm_env = List.assoc tag_num env in
      let l_arg = compile_imm arg1 imm_env in
      let r_arg = compile_imm arg2 imm_env in
      let jump_label = sprintf "less_eq#%d" tag in
      number_op_prologue RAX r_arg @ [IJne (Label "?err_comp_not_num")] @ number_op_prologue RAX l_arg
      @ [ IJne (Label "?err_comp_not_num");
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJle (Label jump_label);
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (Eq, arg1, arg2, tag) ->
      let imm_env = List.assoc tag_num env in
      let l_arg = compile_imm arg1 imm_env in
      let r_arg = compile_imm arg2 imm_env in
      let jump_label = sprintf "eq#%d" tag in
      [ IMov (Reg RAX, l_arg);
        IMov (Reg scratch_reg, r_arg);
        ICmp (Reg RAX, Reg scratch_reg);
        IMov (Reg RAX, const_true);
        IJe (Label jump_label);
        IMov (Reg RAX, const_false);
        ILabel jump_label ]
  | CTuple (immexpr_li, tag) ->
      let imm_env = List.assoc tag_num env in
      let tup_size = List.length immexpr_li in
      let alloc_size_wrds =
        if (tup_size + 1) mod 2 = 0 then
          tup_size + 1
        else
          tup_size + 2
      in
      let mov_size_instr =
        [ IMov (Reg scratch_reg, compile_imm (ImmNum (Int64.of_int tup_size, tag)) imm_env);
          IMov (RegOffset (0, heap_reg), Reg scratch_reg) ]
      in
      let mov_instrs =
        List.concat
          (List.mapi
             (fun i imm ->
               [ IMov (Reg scratch_reg, compile_imm imm imm_env);
                 IMov (RegOffset (8 * (i + 1), heap_reg), Reg scratch_reg) ] )
             immexpr_li )
      in
      let bump_heap_instrs =
        [ IMov (Reg RAX, Reg heap_reg);
          IAdd (Reg RAX, HexConst tuple_tag);
          IAdd (Reg heap_reg, Const (Int64.of_int (8 * alloc_size_wrds))) ]
      in
      reserve (alloc_size_wrds * 8) tag @ 
      mov_size_instr @ mov_instrs @ bump_heap_instrs
  | CSetItem (tup, index, new_val, _) ->
      let imm_env = List.assoc tag_num env in
      let tup_c = compile_imm tup imm_env in
      let index_c = compile_imm index imm_env in
      let new_val_c = compile_imm new_val imm_env in
      let tup_prologue = tuple_op_prologue RAX tup_c in
      let index_prologue = number_op_prologue scratch_reg index_c in
      let check_tuple_and_check_nil =
        [ IJne (Label "?err_get_not_tuple");
          IMov (Reg RAX, tup_c);
          ICmp (Reg RAX, nil_val);
          IJe (Label "?err_nil_deref") ]
      in
      let check_index = [IJne (Label "?err_index_not_num")] in
      let untag_and_cmp =
        [ ISub (Reg RAX, HexConst tuple_tag);
          ICmp (Reg scratch_reg, Const 0L);
          IJl (Label "?err_get_low_index");
          IMov (Reg RAX, RegOffset (0, RAX));
          ICmp (Reg scratch_reg, Reg RAX);
          IJge (Label "?err_get_high_index") ]
      in
      let set_instr =
        [ IMov (Reg RAX, tup_c);
          ISub (Reg RAX, HexConst tuple_tag);
          IMov (Reg scratch_reg, index_c);
          ISar (Reg scratch_reg, Const 1L);
          IAdd (Reg scratch_reg, Const 1L);
          IMul (Reg scratch_reg, Const (Int64.of_int word_size));
          IAdd (Reg scratch_reg, Reg RAX);
          IMov (Reg RAX, new_val_c);
          IMov (RegOffset (0, scratch_reg), Reg RAX); ]
      in
      tup_prologue @ check_tuple_and_check_nil @ index_prologue @ check_index @ untag_and_cmp
      @ set_instr
  | CGetItem (tup, index, _) ->
      let imm_env = List.assoc tag_num env in
      let tup_c = compile_imm tup imm_env in
      let index_c = compile_imm index imm_env in
      let tup_prologue = tuple_op_prologue RAX tup_c in
      let index_prologue = number_op_prologue scratch_reg index_c in
      let check_tuple_and_check_nil =
        [ IJne (Label "?err_get_not_tuple");
          IMov (Reg RAX, tup_c);
          ICmp (Reg RAX, nil_val);
          IJe (Label "?err_nil_deref") ]
      in
      let check_index = [IJne (Label "?err_index_not_num")] in
      let untag_and_cmp =
        [ ISub (Reg RAX, HexConst tuple_tag);
          ICmp (Reg scratch_reg, Const 0L);
          IJl (Label "?err_get_low_index");
          IMov (Reg RAX, RegOffset (0, RAX));
          ICmp (Reg scratch_reg, Reg RAX);
          IJge (Label "?err_get_high_index") ]
      in
      let set_instr =
        [ IMov (Reg RAX, tup_c);
          ISub (Reg RAX, HexConst tuple_tag);
          IMov (Reg scratch_reg, index_c);
          ISar (Reg scratch_reg, Const 1L);
          IMov (Reg RAX, RegOffsetReg (RAX, scratch_reg, word_size, 8)) ]
      in
      tup_prologue @ check_tuple_and_check_nil @ index_prologue @ check_index @ untag_and_cmp
      @ set_instr
  | CPrim2 (CheckSize, e1, e2, _) ->
    let imm_env = List.assoc tag_num env in
    let e1_imm = compile_imm e1 imm_env in 
    let e2_imm = compile_imm e2 imm_env in 
    (tuple_op_prologue RAX e1_imm) @
    [ IJne(Label "?err_get_not_tuple");
      ISub(Reg RAX, HexConst tuple_tag);
      IMov(Reg scratch_reg, RegOffset(0, RAX));
      IMov(Reg RAX, e2_imm);
      ICmp(Reg RAX, Reg scratch_reg);
      IJl(Label "?err_let_tuple_mismatch");
      IMov(Reg RAX, e1_imm);
    ]
  (* Compiling a lambda expression: 
      Structure of lambda instructions: 
       1) 'beginning': Jump to end of lambda for closure construction. Make lambda main label: create stack frame of this 
           lambda as we are now within the call of this lambda. 
       2) 'prologue': create stack frame of this lambda by saving old RBP and creating a new RBP
       3) 'reserveSpace': Push zeroes n times, to allocate stack space for n free variables + local variables. 

       4) make label for body of lambda
       5) 'load_self_and_untag scratch_reg': Put the closure (at [RBP + 16]) into scratch reg
       6) 'restoreFvs scratch_reg': load free variables from the closure into the stack
       7) 'compiledBody': instructions for the actual body of the lambda. 
       8) 'epilogue': destroy stack frame, and return to caller, and then create label for closure construction. 

       9) 'closure information': Instructions for constructing closure for the default part of the closure, i.e. 
          arity, code pointer, number of free vars, and code pointer of lambda body.
       10) 'copy_var_from_arg': Fill out the closure for free variables. 
            *Note* for free variables whose values are closures of mutually recursive lambdas, the actual values will be
            filled in the "patching" process which is done in compilation of ALetRec. Here, we are just putting whatever
            value is CURRENTLY in the location of free var as specified by the env, so for a free var that is a mutually
            recursive lambda that is defined after this lambda that we are compiling, the value will be 0 since 0 is the
            default value that we push on stack frame (go read my notes on ALetRec if this is confusing).
       11) 'reserve heap_alignment t': Perform garbage collection conditionally depending on whether we have enough heap space 
            for the closure that we just allocated.
       12) 'create_closure_value': Once we have made sure to have enough space on heap (either we already had enough, or
           we garbage collected by above step 11), so we should be good), bump up heap pointer by the size of the closure
           that we allocated. 
           *Note* Putting the created closure into stack frame is done at ALetRec, not here. Here we just create the closure in the
           heap.   
  *)
  | CLambda (str_li, body, t) ->
    let prologue = [
      ILineComment("prologue");
      IPush(Reg RBP);
      IMov(Reg RBP, Reg RSP);
      ILineComment("unpack the closure");
    ] in
    let lambda_env = List.assoc t env in
    let closure_env = List.assoc tag_num env in
    let free = (free_vars (ACExpr(e))) in
    (* let free = List.fold_right (fun str set -> StringSet.remove str set) str_li free in *)
    let free = List.sort String.compare free in
    let moveClosureVarToStack idx reg =
      [
        IMov(Reg RAX, RegOffset(32 + 8*idx, reg));
        IMov(RegOffset(~-8 * (idx + 1), RBP), Reg RAX);
      ] in
    let rec push_zeros amt = if amt = 0 then [] else (IPush(Const(0L)))::(push_zeros (amt-1)) in
    let reserveSpace = push_zeros (List.length free + (deepest_stack body lambda_env)) in
    (* let reserveSpace = [ISub(Reg RSP, Const(Int64.of_int (8 * (List.length reserveSpace))))] in *)
    let load_self_and_untag reg = [
      IMov(Reg reg, RegOffset(16, RBP));
      ISub(Reg reg, HexConst(closure_tag));
    ] in
    let restoreFvs reg = List.concat (List.mapi (fun i fv -> moveClosureVarToStack i reg) free) in
    let compiledBody = compile_aexpr body env t (List.length str_li) is_tail in 
    let inner_lambda = sprintf "inner_lambda_%d" t in
    let inner_lambda_body = sprintf "inner_lambda_body_%d" t in
    let inner_lambda_end = sprintf "inner_lambda_end_%d" t in
    let epilogue = [IMov(Reg RSP, Reg RBP); IPop(Reg RBP); IRet; ILabel(inner_lambda_end)] in
    let closure_information = [
      ILineComment("start filling in the closure information");
      IMov(Reg RAX, Const(Int64.of_int (List.length str_li)));
      IShl(Reg RAX, Const(1L));
      IInstrComment(IMov(RegOffset(0, heap_reg), Reg RAX), "arity");
      IMov(Reg RAX, Label(inner_lambda));
      IInstrComment(IMov(RegOffset(8, heap_reg), Reg RAX), "code ptr");
      IMov(Reg RAX, Const(Int64.of_int (List.length free)));
      IInstrComment(IMov(RegOffset(16, heap_reg), Reg RAX), "# vars");
      IMov(Reg RAX, Label(inner_lambda_body));
      IInstrComment(IMov(RegOffset(24, heap_reg), Reg RAX), "code_body ptr");
    ] in
    let moveVarToClosure str i = [
      IMov(Reg RAX, find closure_env str);
      IMov(RegOffset(32 + (8 * i), heap_reg), Reg RAX);
    ] in
    let copy_var_from_arg = List.concat (List.mapi (fun i str -> moveVarToClosure str i) free) in
    let heap_alignment = let closure_length = (List.length free) * 8 + 32 in 
      if closure_length mod 16 = 0 then closure_length else closure_length + 8 in
    let create_closure_value = [
      IMov(Reg RAX, Reg heap_reg);
      IAdd(Reg RAX, HexConst(closure_tag));
      IAdd(Reg heap_reg, Const(Int64.of_int heap_alignment));
    ] in
    let beginning = 
        [
          ILineComment("-- Start of Lambda");
          IJmp(Label inner_lambda_end);
          ILabel(inner_lambda);
        ] 
    in 
    beginning @
    prologue @
    reserveSpace @
    [ILabel(inner_lambda_body)] @
    load_self_and_untag scratch_reg @
    [ILineComment("Restoring free variables")] @
    restoreFvs scratch_reg @
    [ILineComment("actual function body")] @ 
    compiledBody @ 
    epilogue @
    closure_information @
    copy_var_from_arg @
    reserve heap_alignment t @ (* 32 bytes for closure info and then 8 bytes per free var*)
    create_closure_value


and compile_imm (e : tag immexpr) (env : arg name_envt)  =
  match e with
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find env x
  | ImmNil _ -> nil_val
;;




(* This function can be used to take the native functions and produce DFuns whose bodies
   simply contain an EApp (with a Native call_type) to that native function.  Then,
   your existing compilation can turn these DFuns into ELambdas, which can then be called
   as in the rest of Fer-De-Lance, but the Native EApps will do the work of actually
   native_calling the runtime-provided functions. *)
let add_native_lambdas (p : sourcespan program) =
  let wrap_native name arity =
    let argnames = List.init arity (fun i -> sprintf "%s_arg_%d" name i) in
    [DFun(name, List.map (fun name -> BName(name, false, dummy_span)) argnames, EApp(EId(name, dummy_span), List.map(fun name -> EId(name, dummy_span)) argnames, Native, dummy_span), dummy_span)]
  in
  match p with
  | Program(declss, body, tag) ->
    Program((List.fold_left (fun declss (name, (_, arity)) -> (wrap_native name arity)::declss) declss native_fun_bindings), body, tag)

let compile_prog (anfed, (env : arg name_envt tag_envt)) =
  let prelude =
    "section .text
extern ?error
extern ?input
extern ?print
extern ?print_stack
extern ?equal
extern ?try_gc
extern ?naive_print_heap
extern ?HEAP
extern ?HEAP_END
extern ?set_stack_bottom
global ?our_code_starts_here" in
  let suffix = sprintf "
?err_comp_not_num:%s
?err_arith_not_num:%s
?err_logic_not_bool:%s
?err_if_not_bool:%s
?err_overflow:%s
?err_get_not_tuple:%s
?err_get_low_index:%s
?err_get_high_index:%s
?err_nil_deref:%s
?err_out_of_memory:%s
?err_set_not_tuple:%s
?err_set_low_index:%s
?err_set_high_index:%s
?err_call_not_closure:%s
?err_call_arity_err:%s
?err_index_not_num:%s
?err_let_tuple_mismatch:%s
"
                       (to_asm (native_call (Label "?error") [Const(err_COMP_NOT_NUM); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_ARITH_NOT_NUM); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_LOGIC_NOT_BOOL); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_IF_NOT_BOOL); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_OVERFLOW); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_GET_NOT_TUPLE); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_GET_LOW_INDEX); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_GET_HIGH_INDEX)]))
                       (to_asm (native_call (Label "?error") [Const(err_NIL_DEREF); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_OUT_OF_MEMORY); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_SET_NOT_TUPLE); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_SET_LOW_INDEX); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_SET_HIGH_INDEX); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_CALL_NOT_CLOSURE); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_CALL_ARITY_ERR); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_INDEX_NOT_NUMBER); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_LET_TUPLE_MISMATCH); Reg(RAX)]))
  in
  match anfed with
  | AProgram(body, t) ->
  (* $heap and $size are mock parameter names, just so that compile_fun knows our_code_starts_here takes in 2 parameters *)
  let env = 
     if List.mem_assoc t env then env else (t, [])::env in 
     let (prologue, comp_main, epilogue) = 
     compile_fun "?our_code_starts_here" t ocsh_args body env in
     let heap_start =
       [
         ILineComment("heap start");
         IInstrComment(IMov(Sized(QWORD_PTR, Reg(heap_reg)), Reg(List.nth first_six_args_registers 0)), "Load heap_reg with our argument, the heap pointer");
         IInstrComment(IAdd(Sized(QWORD_PTR, Reg(heap_reg)), Const(15L)), "Align it to the nearest multiple of 16");
         IMov(Reg scratch_reg, HexConst(0xFFFFFFFFFFFFFFF0L));
         IInstrComment(IAnd(Sized(QWORD_PTR, Reg(heap_reg)), Reg scratch_reg), "by adding no more than 15 to it");
       ] in
     let set_stack_bottom =
       [
         (* ILabel("?our_code_starts_here"); *)
         IMov(Reg R12, Reg RDI);
       ]
       @ (native_call (Label "?set_stack_bottom") [Reg(RBP)])
       @ [
           IMov(Reg RDI, Reg R12)
         ] in
     let main = (prologue @ set_stack_bottom @ heap_start @ comp_main @ epilogue) in
     sprintf "%s%s%s\n" prelude (to_asm main) suffix
;;

let run_if should_run f =
  if should_run then f else no_op_phase
;;

let compile_to_string ?no_builtins:(no_builtins=false) (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> (add_err_phase well_formed is_well_formed)
  |> (run_if (not no_builtins) (add_phase add_natives add_native_lambdas))
  |> (add_phase desugared desugar)
  |> (add_phase tagged tag)
  |> (add_phase renamed rename_and_tag)
  |> (add_phase anfed (fun p -> atag (anf p)))
  |> (add_phase locate_bindings naive_stack_allocation)
  |> (add_phase result compile_prog)
;;
