open! Stdppx

let has_any_label alist = List.exists alist ~f:(fun (label, _) -> Option.is_some label)

let atom_of_label = function
  | None -> "."
  | Some string -> "~" ^ string
;;
