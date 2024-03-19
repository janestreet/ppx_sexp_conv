open! Base

let is_valid alist = List.exists alist ~f:(fun (option, _) -> Option.is_some option)

let atom_of_label = function
  | None -> "."
  | Some string -> "~" ^ string
;;
