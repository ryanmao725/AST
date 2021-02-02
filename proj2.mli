


val buildParseTree : string list -> Proj2_types.tree
val buildAbstractSyntaxTree : Proj2_types.tree -> Proj2_types.tree

val scanVariable : string list -> string list
val generateInitialAssignList : string list -> (string * bool) list
val generateNextAssignList :
  (string * bool) list -> (string * bool) list * bool
val lookupVar : (string * bool) list -> string -> bool
val evaluateTree : Proj2_types.tree -> (string * bool) list -> bool
val satisfiable : string list -> (string * bool) list list
