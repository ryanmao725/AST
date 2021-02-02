open Proj2_types;;

let isVariable (str : string) : bool = 
    (* Common helper function *)
    match str with
        | "(" -> false
        | ")" -> false
        | "and" -> false
        | "or" -> false
        | "not" -> false
        | "TRUE" -> false
        | "FALSE" -> false
        | _ -> true
;;      

let buildParseTree (input : string list) : tree = 
    let rec parseS (lst : string list) : (tree * string list) = 
        let head = List.hd lst in
        match head with 
            (*
            | "TRUE" -> (TreeNode ("S", [ TreeNode ("TRUE", []) ]), List.tl lst)
            | "FALSE" -> (TreeNode ("S", [ TreeNode ("FALSE", []) ]), List.tl lst) *)
            | "(" -> 
                    (
                    match parseT (List.tl lst) with (t, remaining) ->
                        (TreeNode ("S", [ TreeNode ("(", []) ; t ; TreeNode(")", []) ]), List.tl remaining)
                    )
            | _ -> (TreeNode ("S", [ TreeNode (head, []) ]), List.tl lst)
    and parseT (lst: string list) : (tree * string list) =
        let head = List.hd lst in
        match head with
            | "not" ->
                (
                match parseS (List.tl lst) with (t, remaining) ->
                    (TreeNode ("T", [ TreeNode (head, []); t]), remaining)
                )
            | _ ->
                (
                match parseS (List.tl lst) with (t, remaining) ->
                    match parseS (remaining) with (t2, remaining2) ->
                        (TreeNode ("T", [ TreeNode (head, []); t; t2 ]), remaining2)
                )
    in
    match parseS input with (t, lst) ->
        t
;;

let buildAbstractSyntaxTree (input : tree) : tree =
    let rec dive (t : tree) : tree =
        match t with TreeNode (str, lst) ->
            (
            match str with
                | "T" -> 
                    (
                    let head = List.hd lst in
                    match head with TreeNode (s, ls) ->
                        match s with
                            | "not" -> TreeNode ("not", [dive (List.nth lst 1)])
                            | "and" -> TreeNode ("and", [dive (List.nth lst 1)]@[dive (List.nth lst 2)])
                            | "or" -> TreeNode ("or", [dive (List.nth lst 1)]@[dive (List.nth lst 2)])
                            | _ -> failwith "bad"
                    )
                | "S" -> if (List.length lst) > 1 then dive (List.nth lst 1) else dive (List.hd lst)
                | _ -> TreeNode (str, [])
            )
    in
    dive input
;;

let scanVariable (input : string list) : string list = 
    let getVariablesByFold (res : string list) (s : string) = 
        (* Check to see if array contains the variable already *)
        if (isVariable s) && not (List.mem s res) then res@[s]
        else res
    in
    List.fold_left getVariablesByFold [] input
;;

let generateInitialAssignList (varList : string list) : (string * bool) list =
    let initialAssign (var : string) : string * bool =
        (var, false)
    in
    List.map initialAssign varList
;;

let generateNextAssignList (assignList : (string * bool) list) : (string * bool) list * bool =
    let rec flip (reversedList : (string * bool) list) (carry : bool) (resultList : (string * bool) list) = 
        match reversedList with
            | [] -> resultList
            | (hdVar, hdVal)::tl -> (flip tl (hdVal && carry) resultList)@[(hdVar, (hdVal && not carry) || (not hdVal && carry))]
    in
    let carry (var, value : string * bool) : bool =
        value && true
    in
    let rec generate (assList : (string * bool) list) : ((string* bool) list * bool) = 
        match assList with
            | [] -> ([], true)
            | (hdVar, hdVal)::tl -> 
                    match generate tl with (lst, carry) ->
                        if carry then ([(hdVar, (hdVal && not carry) || (not hdVal && carry))]@lst, carry && hdVal) else ([(hdVar, hdVal)]@lst, carry)
    in
    if assignList = [] then ([], true)
    else generate assignList
;;

let lookupVar (assignList : (string * bool) list) (str : string) : bool =
    let check (var, value : (string * bool)) : bool =
        var = str
    in
    snd (List.find check assignList)
;;
        

let evaluateTree (t : tree) (assignList : (string * bool) list) : bool =
    let rec evaluate (tr : tree) : bool =
        match tr with TreeNode (str, lst) ->
            match str with
                | "not" -> not (evaluate (List.nth lst 0))
                | "and" -> (evaluate (List.nth lst 0)) && (evaluate (List.nth lst 1))
                | "or" -> (evaluate (List.nth lst 0)) || (evaluate (List.nth lst 1))
                | "TRUE" -> true
                | "FALSE" -> false
                | _ -> lookupVar assignList str
    in
    evaluate t
;;

let satisfiable (input : string list) : (string * bool) list list =
    let vars = scanVariable input in
    let initialVarList = generateInitialAssignList vars in
    let parseTree = buildParseTree input in
    let ast = buildAbstractSyntaxTree parseTree in
    let rec loop (t : tree) (assignList : (string * bool) list * bool) (*result : (string * bool) list list*): (string * bool) list list = 
        match assignList with (lst, carry) ->
            if carry then
                []
                (* if evaluateTree t lst then [lst] else [] *)
            else
                let res = loop t (generateNextAssignList lst) in
                if evaluateTree t lst then [lst]@res else res
    in
    let initialAssignList = (initialVarList, false) in
    loop ast initialAssignList
;;
