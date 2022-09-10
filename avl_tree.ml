(*TYPES AND BASES*)

(*Binary tree*)
type 'c btree =
  | N of 'c * 'c btree * 'c btree
  | V
;;
(*Node content*)
type 'a ncontent = {
  height : int;
  elem    : 'a ;
}
;;
(*Mathematical set*)
type 'a set = 'a ncontent btree
(*Comparison (total order)*)
type 'a cmp = 'a -> 'a -> int
;;

(*Exemple AVL tree*)
let ex  =
  N({height = 2; elem = 6},
    N({height = 1; elem = 3},
      N({height = 0; elem = 1}, V, V),
      N({height = 0; elem = 4}, V, V)
     ),
		N({height = 0; elem = 8}, V, V)
   )
;;

(*Function to print the content of a tree*)
let pp_set (pp_elem: 'a -> unit) (s: 'a set) : unit =
  let rec aux (s: 'a set) (b: bool): bool =
    match s with
    | V -> b
    | N(x, g, d) ->
      begin
        let b' = aux g b in
        if b' then (print_string ", ");
        pp_elem x.elem;
        let _ = aux d true in
        true
      end
  in
  print_string "{";
  let _ = aux s false in
  print_string "}" ;;



(*BASIC FUNCTIONS*)

(*Get height of a node*)
let get_height (node_cont : 'a ncontent) : int =
	node_cont.height ;;
(*Get element of a node*)
let get_key (node_cont : 'a ncontent) : 'a =
	node_cont.elem ;;
(*Get height of a tree*)
let height (avl : 'a set) : int =
	match avl with
	| V -> -1 ;
	| N (node_content, _, _) -> get_height node_content ;;


(*ADVANCED FUNCTIONS*)

(*Build a tree, given root element and sons*)
let build_tree (node_content : 'a ncontent) (l : 'a set) (r : 'a set) : 'a set =
	let h = 1 + max (height l) (height r) in
	N({height = h; elem = get_key node_content}, l, r) ;;	
(*Get the balance (sons height difference) of a tree*)
let get_balance (avl : 'a set) : int =
	match avl with
	| V -> 0 ;
	| N(_, l, r) -> height l - height r ;;
(*Check if element is in tree*)
let mem (comp : 'a cmp) (num : 'a) (avl : 'a set) : bool =
	let rec rec_mem (comp : 'a cmp) (num : 'a) (avl : 'a set) : bool =
		match avl with
		| V -> false
		| N(x, _, _) when (comp num (get_key x)) = 0 -> true
		| N(x, l, _) when (comp num (get_key x)) < 0 -> rec_mem comp num l
		| N(x, _, r) when (comp num (get_key x)) > 0 -> rec_mem comp num r
		| _ -> false
	in rec_mem comp num avl;;
(*Tree to ordrered list of elements (need ordering function, must be total order)*)
let elements (avl : 'a set) (comp : 'a cmp): 'a list =
  let rec rec_elements (avl : 'a set) (comp : 'a cmp) (avl_list : 'a list) : 'a list =
    match avl with
    | V -> avl_list
    | N(x, l, r) -> rec_elements l comp (x.elem :: rec_elements r comp avl_list)
	in rec_elements avl comp [];;



(*BALANCING FUNCTIONS*)

(*Exception used when a tree is not in a good configuration to be rotated*)
exception WrongRotationPattern of int set;;
(*Rotate a tree to the left*)
let left_rot (avl : 'a set) : 'a set =
  match avl with
  | N(x, l, N(y, l', r')) -> build_tree y (build_tree x l l') r'
  | _ -> raise (WrongRotationPattern avl) ;;
(*Rotate a tree to the right*)
let right_rot (avl : 'a set) : 'a set =
  match avl with
  | N(x, N(y, l', r'), r) -> build_tree y l' (build_tree x r' r)
  | _ -> raise (WrongRotationPattern avl) ;;
(*Rotate a tree to the left then to the right*)
let left_right_rot (avl : 'a set) : 'a set =
  match avl with
  | N(x, l, r) -> left_rot (build_tree x l r)
  | _ -> raise (WrongRotationPattern avl) ;;
(*Rotate a tree to the right then to the left*)
let right_left_rot (avl : 'a set) : 'a set =
  match avl with
  | N(x, l, r) -> right_rot (build_tree x l r)
  | _ -> raise (WrongRotationPattern avl) ;;
(*Balance a tree (note : the balance of a tree is always in [-2,2] before being balanced)*)
let balance (avl : 'a set) : 'a set =
  match avl with
  | N(x, l, r) when (get_balance avl) = 2 ->
    begin
      match l with
      | N(y, _, _) when (get_balance l) = 1 -> right_rot avl
      | N(y, _, _) when (get_balance l) = -1 -> left_right_rot avl
      | _ -> raise (WrongRotationPattern avl)
    end
  | N(x, l, r) when (get_balance avl) = -2 ->
    begin
      match r with
      | N(y, _, _) when (get_balance r) = -1 -> left_rot avl
      | N(y, _, _) when (get_balance r) = 1 -> right_left_rot avl
      | _ -> raise (WrongRotationPattern avl)
    end
  | _ -> avl ;;
  
