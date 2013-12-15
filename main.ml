open Syntax;;

let rec remove_dups lst= match lst with 
    | [] -> []
    | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))
;;

let rec make_call_graph_expr l = 
    match l with
    | Un(u,e) -> make_call_graph_expr e
    | Bin(b,e1,e2) -> (make_call_graph_expr e1) @ (make_call_graph_expr e2)
    | Function_call(s,e::el) -> (make_call_graph_expr e) @ (make_call_graph_expr (Function_call(s,el))) @ [s]
    | Function_call(s,_) -> [s]
    | Geti(e1,e2) -> (make_call_graph_expr e1) @ (make_call_graph_expr e2)
    | Read -> ["read"]
    | Readln -> ["readln"]
    | _ -> []
;;

let rec make_call_graph_instr l = 
    match l with
    | Sequence(i::l) ->  (make_call_graph_instr i) @ (make_call_graph_instr (Sequence(l)))
    | Set(s,e) -> (make_call_graph_expr e)
    | If(e,i1,i2) -> (make_call_graph_expr e) @ (make_call_graph_instr i1) @ (make_call_graph_instr i2)
    | While(e,i) -> (make_call_graph_expr e) @ (make_call_graph_instr i)
    | Procedure_call(s,e::el) -> (make_call_graph_expr e) @ (make_call_graph_instr (Procedure_call(s,el))) @ [s]
    | Procedure_call(s,_) -> [s]
    | Write(e) -> ["write"] @ (make_call_graph_expr e)
    | Writeln(e) -> ["writeln"] @ (make_call_graph_expr e)
    | Seti(e1,e2,e3) -> (make_call_graph_expr e1) @ make_call_graph_expr e2 @ (make_call_graph_expr e3)
    | _ -> []
;;

let rec make_call_graph_definition l hash =
    match l with
    | (s,(v,t,vl,i))::r -> make_call_graph_definition r hash ; 
                           (List.iter (Hashtbl.add hash s) (remove_dups(make_call_graph_instr i)))
    | _ -> ()
;;

let make_call_graph p =
    let my_hash = Hashtbl.create 100 in 
        match p with
        | (v,l,i) ->  make_call_graph_definition l my_hash ; 
                      (List.iter (Hashtbl.add my_hash "program") (remove_dups(make_call_graph_instr i))) ; 
                      my_hash
;;

let print_call_graph_couple i f init =
    init^"  "^i^" -> "^f^";\n"
;;

let print_call_graph cg=
    print_string ("digraph program {\n"^(Hashtbl.fold print_call_graph_couple cg "")^"}\n")
;;

let main =
  try
  let ic = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel ic in
        let pgm = Parser.input Lexer.token lexbuf in print_call_graph (make_call_graph pgm)
  with End_of_file -> exit 0
;;

main;;