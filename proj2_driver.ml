open Proj2_types;;
open Proj2;;

let tokenListFromString (str : string) = Str.split (Str.regexp_string " ") str;;

let parseTreeFromString (str : string) = buildParseTree (tokenListFromString str);;

let astFromString (str : string) = buildAbstractSyntaxTree (parseTreeFromString str);;

let satisfiableFromString (str : string) = satisfiable (tokenListFromString str);;

let rec printTreeInternal (t : tree) (n : int) = match t with TreeNode (str, lst) ->
  let space = String.make n ' ' in
  print_endline (space ^ "TreeNode (\"" ^ str ^ "\", [");
  List.iter (fun x -> printTreeInternal x (n + 2)) lst;
  print_endline (space ^ "]);");;

let printTree (t : tree) = printTreeInternal t 0;;
