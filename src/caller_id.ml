
(* Small helper to find out who is the caller of a function *)

module Printexc = Caml.Printexc

type t = Printexc.location option

let get ~skip =
  let skip = Caml.__FILE__ :: skip in
  let stack = Printexc.get_callstack 16 in
  let len = Printexc.raw_backtrace_length stack in
  let rec loop pos =
    if Int.equal pos len then
      None
    else
      match
        Printexc.get_raw_backtrace_slot stack pos
        |> Printexc.convert_raw_backtrace_slot
        |> Printexc.Slot.location
      with
      | None -> None
      | Some loc ->
        if List.mem skip loc.filename then
          loop (pos + 1)
        else
          Some loc
  in
  loop 0
;;
