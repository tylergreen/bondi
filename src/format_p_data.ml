

(*** formatting *)

let pf s = Printf.printf "%s\n" s;flush stdout ;;
let ps = Format.print_string;;
let lpn() = ps "(" ;;
let rpn() = ps ")" ;;


(* tidying - general, but first used with functors *)

let incrStringCounter ctr minc maxc = (* for incrementing term and type variables *)
  let ndx = ref (String.length ctr - 1)
  and flag = ref false
  and newCtr = String.copy ctr
  in 
  while (!ndx >= 0 & !flag = false) do
    flag := true;
    let c = Char.chr ((Char.code newCtr.[!ndx]) + 1)
    in
    if c <= maxc
    then 
      newCtr.[!ndx] <- c
    else (* carry *) 
      (flag := false;
       newCtr.[!ndx] <- minc;
       ndx := !ndx - 1)
  done;
  if (!flag = false) (* need to extend string *)
  then
    (String.make 1 minc) ^ newCtr
  else
    newCtr
;;

let rec format_identifier bound str = 
  if List.mem str bound 
  then ps "'";
  ps str 
;;

let string_of_tyvar = 
  function 
    TyVar s -> s 
  | MTypeVar n -> "ty_"^(string_of_int n)
;;

let rec format_p_type = function

    PtyV x -> ps (string_of_tyvar x) 

  | Pconstant x -> ps x

  | PapplyF(ty1,ty2) ->
      format_p_type ty1;
      ps " ";
      format_p_type ty2 
	
  | Pfunty (ty1,ty2) ->
      format_p_type ty1;
      ps " -> ";
      format_p_type ty2 
	
  | Plinty ty ->
      ps "lin "; 
      format_p_type ty
	
  | Pquant (x,ty2) ->
      ps ("all "^(string_of_tyvar x)^".");
      format_p_type ty2 

  | Pnestedclass(str,tys,ty2) -> 
      ps (str^ "<"); 
      format_p_types tys;
      ps ">" ;
      ps "["; 
      format_p_type ty2; 
      ps "]"
  | Pref ty -> 
      ps "ref " ; 
      format_p_type ty

  | Parr ty -> 
      ps "array " ; 
      format_p_type ty

and format_p_types = function
  [] -> ()
  | [ty] -> format_p_type ty 
  | ty :: tys1 -> format_p_type ty ; ps "," ; format_p_types tys1
;;

let p_peek_type pty msg = 
  format_p_type pty; 
  print_flush(); 
  pf (" is " ^msg) ; 
  print_flush()
;;



(*** term formatting *) 

let rec format_identifiers = function 
    [] -> ()
  | [x] -> ps x 
  | x:: xs -> ps x; ps ","; format_identifiers xs
   

let rec format_p_term = function

   | Ptvar x 
   | Pconstructor x -> ps x
   | Pwildcard str -> ps ("_"^str)
   | Pdatum d -> ps (string_of_datum_value d)

   | Poper(str,ts) -> 
       ps str;
       let f t = lpn(); format_p_term t; rpn() in 
       List.iter f ts

   | Papply(f,u) -> 
       lpn(); 
       format_p_term f; 
       rpn();       
       lpn(); 
       format_p_term u; 
       rpn()
	 
   | Plam (p,s) ->
       ps "fun ";
       format_p_term p;
       ps " -> ";
       format_p_term s
	 
   | Plin (p,s) ->
       ps "fun ";
       format_p_term p;
       ps " --> ";
       format_p_term s
	 
   | Pcases cases -> 
       let rec format_case (xs_opt,p,_,s) = 
	 match xs_opt with 
	   None -> 
	     format_p_term p;
	     ps " -> ";
	     format_p_term s
	 | Some xs -> 
	     ps "{";
	     iter (fun x -> ps x; ps ",") xs;
	     ps "} ";
	     format_p_term p;
	     ps " -> ";
	     format_p_term s
       in 
       lpn();
       List.iter format_case cases; 
       rpn()

   | Paddcase (x,case) ->
     ps (x ^ " += ");
     format_p_term (Pcases[case])
	 
   | Plet (x,t1,t2) ->
       ps "let ";
       format_p_term x;
       ps " = "; 
       format_p_term t1;
       ps " in ";
       format_p_term t2;

   | Pletrec (x,t1,t2) ->
       ps "let rec ";
       format_p_term x;
       ps " = "; 
       format_p_term t1;
       ps " in ";
       format_p_term t2;
       
   | Pletext (x,t1,t2) ->
       ps "let ext ";
       format_p_term x;
       ps " = "; 
       format_p_term t1;
       ps " in ";
       format_p_term t2;

   | Pletmethod (x,t1,t2) ->
       ps "let method ";
       format_p_term x;
       ps " = "; 
       format_p_term t1;
       ps " in ";
       format_p_term t2;

   | Ptyped (t'',ty) ->
       lpn();
       format_p_term t'' ;
       ps " : ";
       format_p_type ty;
       rpn()
	 
   | Pnew (str,tys) -> 
       ps "new ";
       ps str; 
       ps "<" ;
       format_p_types tys;
       ps ">"
	 
   | Pinvoke (t,x,super) -> 
       format_p_term t ;
       if super 
       then ps ".super"
       else () ;
       ps ("."^x)
    
   | PnewArr(t,n)  -> 
       ps "newarray ";
       format_p_term t;
       ps " ";
       format_p_term n

and p_peek t str = 
  format_p_term t ; 
  print_flush(); 
  pf (" is " ^ str);
  print_flush()

and p_peeks ts str = List.iter (fun x -> p_peek x str) ts 


let formatPTermError (ts,s) = 

  ps ("term error: ");
 
  let form_in_box t =
    try 
      format_p_term t;
    with _ -> pf "cannot format term error"
  in

  match ts with
    [t] ->
      form_in_box t;
      ps (" "^s);
      print_newline()

  | [t1;t2] ->
      form_in_box t1;
      ps " and ";
      form_in_box t2;
      ps (" "^s);
      print_newline()

  | [t1;t2;t3] ->
      form_in_box t1;
      ps " and ";
      form_in_box t2;
      ps " and ";
      form_in_box t3;
      ps (" "^s);
      print_newline()

  | _ -> pf "unformatted term error"
;;


