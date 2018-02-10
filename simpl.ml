(* Ali Ghanbari *)
(* axg173030@utdallas.edu *)

open Simpltypes

(* The 'store' models the machine's memory as a mapping from
 * variable names to integers. *)
type store = varname -> int

(* auxiliary function defintions *)
(* citation: this is taken from the previous assignment *)
let update f x y = fun x' -> if x' = x then y else f x';;

let init_store (l : (varname * int) list) : store =
  let f acc (var, vl) = update acc var vl in (* OMG! val is a keyword of OCaml! *)
    List.fold_left f (fun v -> raise (Failure "oops! uninitialized variable")) l;;

let rec eval_arith (s : store) (a : iarith) : int =
  match a with
    Const n         -> n
  | Var v           -> s v
  | Plus (al, ar)   -> (eval_arith s al) + (eval_arith s ar)
  | Minus (al, ar)  -> (eval_arith s al) - (eval_arith s ar)
  | Times (al, ar)  -> (eval_arith s al) * (eval_arith s ar);;

let rec eval_bool (s : store) (b : ibool) : bool =
  match b with
    True          -> true
  | False         -> false
  | Leq (al, ar)  -> (eval_arith s al) <= (eval_arith s ar)
  | Conj (bl, br) -> (eval_bool s bl) && (eval_bool s br)
  | Disj (bl, br) -> (eval_bool s bl) || (eval_bool s br)
  | Neg be        -> not (eval_bool s be);;

let rec exec_cmd (s : store) (c : icmd) : store =
  match c with
  | Skip              -> s
  | Seq (c1, c2)      -> exec_cmd (exec_cmd s c1) c2
  | Assign (var, ae)  -> update s var (eval_arith s ae)
  | Cond (be, ct, ce) -> if (eval_bool s be) then exec_cmd s ct else exec_cmd s ce
  | While (g, cb)     -> exec_cmd s (Cond (g, Seq (cb, While (g, cb)), Skip));;


(* This is the main entrypoint of the code. You don't need to understand
 * how it works to complete the assignment, but here's a short explanation
 * for those interested:
 *  * The first 'let' statement reads the .sim file and invokes an external
 *    parser (defined in simplparser.mly) to convert the file into an icmd
 *    structure.
 *  * The next 'let' statement calls your init_store code to create a
 *    store s from the rest of the command-line arguments. The first
 *    command-line argument gets assigned to variable "arg0", the next to
 *    "arg1", etc.
 *  * The third 'let' statement calls your exec_cmd code to execute the icmd
 *    starting in the initial store.
 *  * If the SIMPL program terminates (and your code is correct) then s2 will
 *    eventually hold the final store that results from executing the
 *    program. We then print out the value of variable "ret" as the result
 *    of the computation.
 *)
let main () =
  (match (Array.to_list Sys.argv) with
     [] -> raise (Sys_error "invalid argument list")
   | [_] -> raise (Failure "please specify a program to interpret")
   | _::prog::args ->
      let c = (Simplparser.parse_cmd Simpllexer.token 
                (Lexing.from_channel (open_in prog))) in
      let s = init_store (Array.to_list (Array.mapi
                (fun i a -> ("arg"^(string_of_int i),
                  (try int_of_string a
                   with Failure _ -> raise (Failure "args must be ints"))))
                (Array.of_list args))) in
      let s2 = exec_cmd s c in
        (print_string (match (try Some (s2 "ret") with Not_found -> None) with
                         None -> "<no value returned>"
                       | Some n -> string_of_int n);
         print_newline ()));;

main ();;

