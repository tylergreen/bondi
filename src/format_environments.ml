let format_datum d = ps (string_of_datum_value d)

let rec format_term_variable = function
  | Var y -> ps y
  | Mvar n  -> ps ("x_" ^(string_of_int n))
;;

let format_sEnv sub sEnv = 
  let fs x ty = 
    format_term_variable x; 
    ps ":";
    format_type false (applySub sub ty);
    print_newline()
  in 
  ps "["; 
  TMap.iter fs sEnv;
  pf "]" ;
  print_newline();
  print_flush()
;;


let rec do_format t' left_prec right_prec =
  match t' with
  | Tvar (x,n) -> 
      format_term_variable x; 
      if get_mode "declaration_index" = Show_on then printf "_%d" n
  | Tsuper (x,_) -> ps "super."; format_term_variable x
  | Twildcard str -> ps ("_"^str)
  | Tconstructor (x,n) ->  
      format_term_variable x; 
      if get_mode "declaration_index" = Show_on then printf "_%d" n
  | Datum d -> ps (string_of_datum_value d)
	
  | Oper("cond",[t1;t2;t3]) ->
      ps ("(if " ) ;
      do_format t1 0 0;
      ps " then ";
      do_format t2 0 0;
      ps " else ";
      do_format t3 0 0;
      ps ")"
	
  | Oper("assign",[t1;t2]) -> 
      do_format t1 0 0;
      pf " = "; 
      do_format t2 0 0
	
  | Oper(str,args) -> ps (str^"("); format_arg_list  args; ps ")" 

  | Apply(t1,t2) ->
      begin
        let needParens = prec_app <= left_prec || prec_app < right_prec in
        open_box 0;
        do_format t1 left_prec prec_app;
        print_space();
        if needParens 
	then begin
	  lpn();
          do_format t2 prec_app 0; 
          rpn()
        end 
	else do_format t2 prec_app right_prec;
        close_box()
      end
	
  | Lam(x,s) -> 
      lpn();
      format_term_variable x ;
      ps " -> " ;
      do_format s 0 0;
     rpn()	

  | Case (None,p,s) -> 
      lpn();
      do_format p 0 0 ;
      ps " -> ";                 
      do_format s 0 0;
      rpn()

  | Case (Some theta,p,s) -> 
      lpn();
      ps "{";
      List.iter (fun x -> 
	format_term_variable x;
		ps ",") theta ;
      ps "}";
      do_format p 0 0 ;
      ps " -> ";                 
      do_format s 0 0;
      rpn();
 
  | Choice (s,t) -> 
      do_format s left_prec prec_app;
      ps " | "; 
      do_format t prec_app right_prec

  | Addcase (x,t,_) -> 
      lpn();
      format_term_variable x ;
      ps " += " ;
      do_format t 0 0;
      rpn()

  | Tlet (x,t1,t2) ->
      ps "let ";
      format_term_variable  x;
      ps " = "; 
      do_format t1 0 0;
      ps " in ";
      do_format t2 0 right_prec;
      
  | Tletrec (x,t1,t2) ->
      ps "letrec ";
      format_term_variable  x;
      ps " = "; 
      do_format t1 0 0;
      ps " in ";
      do_format t2 0 right_prec;
      
  | Tletext (x,t1,t2) ->
      ps "letext ";
      format_term_variable  x;
      ps " = "; 
      do_format t1 0 0;
      ps " in ";
      do_format t2 0 right_prec;
      
  | TnewArr (t,n) -> 
      ps "newArray ";
      do_format t 0 0;
      ps " " ;
      do_format n 0 0

and format_arg_list = function
    [t] -> do_format t 0 0
  | t::ts -> do_format t 0 0 ; ps ","; format_arg_list ts 
  | [] -> ()
	
and format_closure clos str = 
  let f x y = 
    format_term_variable x;
    ps " |-> ";
    format_term_variable y;
    ps " " ;
    flush stdout;
  in 
  TMap.iter f clos;
  pf (" is "^str)
    
let format_term t = 
  open_box 0;
  do_format t 0 0;
  close_box(); 
  print_flush()
;;



(* formatting values *)


let rec format_value = function 
  | Vvar x -> format_term_variable x
  | Vsuper x -> format_term_variable x; ps ".super"
  | Vwildcard str -> ps ("_"^str)
  | Vconstructor (x,n) -> format_term_variable x 
  | Vdatum d -> ps (string_of_datum_value d)
  | Vapply(x1,x2) ->
      format_term_variable x1;
      print_space();
      format_term_variable x2;
  | Vchoice(x1,x2) ->
      format_term_variable x1;
      ps " | ";
      format_term_variable x2;
  | _ -> ps "..."





(* peeking *)

let peek_type ty msg = 
  format_type false ty; 
  print_flush(); 
  pf (" is " ^msg) ; 
  print_flush()
;;

let peek_tyvs delta msg = 
pf "[";
iter (fun x ->   format_type false (TyV(x,0)); pf ","; print_flush() ) delta;
pf ("] is "^msg);
  print_flush()
;;


let peek t str = format_term t; print_flush(); pf (" is " ^ str)

let peeks ts str = List.iter (fun x -> peek x str) ts 

let peek_value v str = format_value v; print_flush(); pf (" is " ^ str)




(*** Errors *)

let formatTypeError sub (tys,s) = 

  let form_in_box ty = 
    open_box 2;
    format_type false (applySub sub ty);
    close_box()
  in

  match tys with 
    [ty] -> 
      open_box 0;
      ps ("type error: ");
      form_in_box ty;
      ps (" "^s);
      close_box();
      print_newline()

  | [ty1;ty2] -> 
      open_box 0;
      ps ("type error: ");
      form_in_box ty1;
      ps " and ";
      form_in_box ty2;
      ps (" "^s);
      close_box();
      print_newline()

  | [ty1;ty2;ty3] -> 
      open_box 0;
      ps ("type error: ");
      form_in_box ty1;
      ps " and ";
      form_in_box ty2;
      ps " and ";
      form_in_box ty3;
      ps (" "^s);
      close_box();
      print_newline()

  | _ -> pf "unformatted type error"
;;

let formatTermError (ts,s) = 

  let form_in_box t =
try 
    open_box termIndent;
    format_term t;
    close_box()
with _ -> pf "cannot format term error"
  in

  match ts with
    [t] ->
      open_box 0;
      ps ("term error: ");
      form_in_box t;
      ps (" "^s);
      close_box();
      print_newline()

  | [t1;t2] ->
      open_box 0;
      ps ("term error: ");
      form_in_box t1;
      ps " and ";
      form_in_box t2;
      ps (" "^s);
      close_box();
      print_newline()

  | [t1;t2;t3] ->
      open_box 0;
      ps ("term error: ");
      form_in_box t1;
      ps " and ";
      form_in_box t2;
      ps " and ";
      form_in_box t3;
      ps (" "^s);
      close_box();
      print_newline()

  | _ -> pf "unformatted term error"
;;



