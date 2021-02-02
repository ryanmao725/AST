open Proj2_types;;
open Proj2;;

let rec compareTree (tree1 : tree) (tree2 : tree) : bool =
  match tree1 with TreeNode (str1, lst1) ->
  match tree2 with TreeNode (str2, lst2) ->
  str1 = str2 && List.length lst1 = List.length lst2 && List.fold_left2 (fun p x y -> p && compareTree x y) true lst1 lst2;;

let cmpList (lst1 : 'a list) (lst2 : 'a list) (cmp : 'a -> 'a -> bool) : bool =
  List.length lst1 = List.length lst2 && List.fold_left (fun p x -> p && List.exists (cmp x) lst2) true lst1;;

let cmpSatisfiable lst1 lst2 = cmpList lst1 lst2 (fun x y -> cmpList x y (=));;

let inputExpr = ["(";"and";"(";"or";"a";"b";")";"TRUE";")"];;

let tree1 =
TreeNode ("S",
 [TreeNode ("(", []);
  TreeNode ("T",
   [TreeNode ("and", []);
    TreeNode ("S",
     [TreeNode ("(", []);
      TreeNode ("T",
       [TreeNode ("or", []); TreeNode ("S", [TreeNode ("a", [])]);
        TreeNode ("S", [TreeNode ("b", [])])]);
      TreeNode (")", [])]);
    TreeNode ("S", [TreeNode ("TRUE", [])])]);
  TreeNode (")", [])]);;

let tree2 = Proj2.buildParseTree inputExpr;;

if compareTree tree1 tree2 then print_endline "buildParseTree passed" else print_endline "buildParseTree failed";;

let ast1 = TreeNode ("and",
 [TreeNode ("or", [TreeNode ("a",[]);TreeNode ("b",[])]);
  TreeNode ("TRUE", [])]);;

let ast2 = Proj2.buildAbstractSyntaxTree tree1;;

if compareTree ast1 ast2 then print_endline "buildAbstractSyntaxTree passed" else print_endline "buildAbstractSyntaxTree failed";;

let varList1 = ["a";"b"];;

let varList2 = Proj2.scanVariable inputExpr;;

if cmpList varList1 varList2 (=) then print_endline "scanVariable passed" else print_endline "scanVariable failed";;

let initAssign1 = [("a",false);("b",false)];;

let initAssign2 = Proj2.generateInitialAssignList ["a";"b"];;

if cmpList initAssign1 initAssign2 (=) then print_endline "generateInitialAssignList passed" else print_endline "generateInitialAssignList failed";;

let nextAssign1 = [("a",false);("b",true)];;
let nextAssign2 = [("a",true);("b",false)];;

let nextAssign3 = Proj2.generateNextAssignList [("a",false);("b",false)];;
let nextAssign4 = Proj2.generateNextAssignList [("a",false);("b",true)];;
let nextAssign5 = Proj2.generateNextAssignList [("a",true);("b",true)];;

if (snd nextAssign3) || (snd nextAssign4) || not (snd nextAssign5) then print_endline "generateNextAssignList failed"
else if cmpList nextAssign1 (fst nextAssign3) (=) && cmpList nextAssign2 (fst nextAssign4) (=) then print_endline "generateNextAssignList passed" else print_endline "generateInitialAssignList failed";;

if not (Proj2.lookupVar [("a",false);("b",true)] "a") then print_endline "lookupVar passed" else print_endline "lookupVar failed";;

if not (Proj2.evaluateTree ast1 [("a",false);("b",false)]) && Proj2.evaluateTree ast1 [("a",true);("b",false)] then print_endline "evaluateTree passed" else print_endline "evaluateTree failed";;

let satResult1 = [[("a",false);("b",true)];[("a",true);("b",false)];[("a",true);("b",true)]];;
let satResult2 = Proj2.satisfiable inputExpr;;

if cmpSatisfiable satResult1 satResult2 then print_endline "satisfiable testcase 1 passed" else print_endline "satisfiable failed";;

let satResult3 = [];;
let satResult4 = Proj2.satisfiable ["(";"and";"a";"(";"not";"a";")";")"];;

if cmpSatisfiable satResult3 satResult4 then print_endline "satisfiable testcase 2 passed" else print_endline "satisfiable failed";;

let satResult5 = [[("a",false);("b",false)];[("a",true);("b",false)];[("a",false);("b",true)];[("a",true);("b",true)]];;
let satResult6 = Proj2.satisfiable ["(";"not";"(";"and";"(";"or";"a";"b";")";"FALSE";")";")"];;

if cmpSatisfiable satResult5 satResult6 then print_endline "satisfiable testcase 3 passed" else print_endline "satisfiable failed";;

let satResult7 = [[("a",true);("b",true);("c",true)]];;
let satResult8 = Proj2.satisfiable ["(";"not";"(";"or";"(";"or";"(";"not";"a";")";"(";"not";"b";")";")";"(";"not";"c";")";")";")"];;

if cmpSatisfiable satResult7 satResult8 then print_endline "satisfiable testcase 4 passed" else print_endline "satisfiable failed";;

let satResult9 = [[("a",true);("b",true)];[("a",false);("b",false)]];;
let satResult10 = Proj2.satisfiable ["(";"and";"(";"or";"a";"(";"not";"b";")";")";"(";"or";"b";"(";"not";"a";")";")";")"];;

if cmpSatisfiable satResult9 satResult10 then print_endline "satisfiable testcase 5 passed" else print_endline "satisfiable failed";;
