val format_p_type : p_type -> unit
val format_p_term : p_term -> unit
val formatPTermError :  p_term list * string -> unit

val p_peek_type : p_type -> string -> unit
val p_peek : p_term -> string -> unit
val p_peeks : p_term list -> string -> unit

