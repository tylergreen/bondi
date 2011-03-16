(* BONDI version must be moved to ../VERSION *)

let version = "2.04" 
let standard_library_default = "/usr/lib/bondi"
let standard_library =
  try
    Sys.getenv "BONDI_LIB_DIR"
  with Not_found ->
  try
    Sys.getenv "BONDILIB"
  with Not_found ->
    standard_library_default

let architecture = "i686"
let model        = "pc"
let system       = "linux-gnu"
;;
