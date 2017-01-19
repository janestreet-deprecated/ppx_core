open Glue
include Base
include Stdio

(* This is not re-exported by Base and we can't use [%here] in ppx_core *)
external __FILE__ : string = "%loc_FILE"
