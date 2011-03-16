val format_tyvarset : TyVarSet.t -> unit
val format_tyvarlist : P_data.tyVar list -> unit
val format_sub : sub -> unit
val format_sEnv : sub -> i_type TMap.t -> unit
val format_term_variable :term_variable -> unit
val format_term : i_term -> unit
val format_value : value -> unit
val formatTypeError :
    i_type TyMap.t -> i_type list * string -> unit
val formatTermError :  i_term list * string -> unit

val peek_type : i_type -> string -> unit 
val peek_tyvs : P_data.tyVar list  -> string -> unit 
val peek : i_term -> string -> unit
val peeks : i_term list -> string -> unit
val peek_value : value -> string -> unit

