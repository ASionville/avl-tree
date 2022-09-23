type 'c btree =
  | N of 'c * 'c btree * 'c btree (* arbre contenant un noeud *)
  | V                             (* arbre vide *)

type 'a ncontent = {
  hauteur : int;                (* hauteur du noeud courant *)
  elem    : 'a ;                (* élément du noeud courant *)
}

type 'a set = 'a ncontent btree

let ex  =
  N({hauteur = 2; elem = 6},
    N({hauteur = 1; elem = 3},
      N({hauteur = 0; elem = 1}, V, V),
      N({hauteur = 0; elem = 4}, V, V)
     ),
    N({hauteur = 0; elem = 8}, V, V)
   )

type 'a cmp = 'a -> 'a -> int

let get_height (x: 'a ncontent): int =
  x.hauteur

let get_key (x: 'a ncontent) : 'a =
  x.elem

let height (x: 'a set): int =
  match x with
  | N(nc, _, _) -> get_height nc
  | V -> -1

(* construit un noeud en recalculant la hauteur de la racine *)
let cons_arbre (x: 'a ncontent) (g: 'a set) (d: 'a set): 'a set =
  let new_h = 1 + max (height g) (height d) in
  N({x with hauteur = new_h}, g, d)

(* calcule l'équilibrage des noeuds *)
let equilibrage (x: 'a set): int =
  match x with
  | N(_, g, d) -> (height d) - (height g)
  | V -> -1

(* teste l'appartenance d'une étiquette à un ensemble *)
let rec mem cmp (x: 'a) (s: 'a set) : bool =
  match s with
  | V -> false
  | N({elem = y}, g, d) ->
    let c = cmp x y in
    if c > 0 then mem cmp x d
    else if c < 0 then mem cmp x g
    else true

(* retourne la liste triée des éléments d'un ensemble avec une complexité linéaire, non RT *)
let elements (s: 'a set): 'a list =
  let rec aux (s: 'a set) (res: 'a list): 'a list =
    match s with
    | V -> res
    | N({elem=y}, g, d) ->
      let res1 = aux g res in
      let res2 = y :: res1 in
      let res3 = aux d res2 in
      res3
  in
  aux s [] |> List.rev

type 'a todoitem =
  | Full of 'a set
  | Half of 'a * 'a set

(* retourne la liste triée des éléments d'un ensemble avec une complexité linéaire, RT *)
let elements_tr (s: 'a set): 'a list =
  let rec aux (todo: 'a todoitem list) (res: 'a list): 'a list =
    match todo with
    | [] -> res
    | Full(V)::todo' -> aux todo' res
    | Half(x, d)::todo' -> aux (Full(d) :: todo') (x :: res)
    | Full(N({elem=y}, g, d))::todo' -> aux (Full(g) :: Half(y, d) :: todo') (res)
  in
  aux [Full(s)] []
  |> List.rev

(* les rotations *)
let rotation_gauche (s: 'a set) : 'a set =
  match s with
  | N(x, alpha, N(y, beta, gamma)) -> cons_arbre y (cons_arbre x alpha beta) gamma
  | _ -> assert false

let rotation_droite (s: 'a set) : 'a set =
  match s with
  | N(y, N(x, alpha, beta), gamma) -> cons_arbre x alpha (cons_arbre y beta gamma)
  | _ -> assert false

let rotation_droite_gauche (s: 'a set) : 'a set =
  match s with
  | N(z, alpha, N(y, N(x, beta, gamma), epsilon)) -> cons_arbre x (cons_arbre z alpha beta) (cons_arbre y gamma epsilon)
  | _ -> assert false

let rotation_gauche_droite (s: 'a set) : 'a set =
  match s with
  | N(z, N(y, alpha, N(x, beta, gamma)), epsilon) -> cons_arbre x (cons_arbre y alpha beta) (cons_arbre z gamma epsilon)
  | _ -> assert false

(* procède au rééquilibrage d'un noeud, au moyen de rotations *)
let reequilibre (s: 'a set): 'a set =
  match s, equilibrage s with
  | N(_, _, d), 2 -> (match equilibrage d with
      | 1 | 0 -> rotation_gauche s
      | _ -> rotation_droite_gauche s
    )
  | N(_, g, _), -2 -> (match equilibrage g with
      | -1 | 0 -> rotation_droite s
      | _ -> rotation_gauche_droite s
    )
  | _ -> s

(* insertion dans un avl *)
let rec insertion cp (x: 'a) (s: 'a set): 'a set =
  match s with
  | V -> N({hauteur = 0; elem = x}, V, V)
  | N(({elem = y} as nc), g, d) ->
    let c = cp x y in
    if c > 0 then
      cons_arbre nc g (insertion cp x d)
      |> reequilibre
    else if c < 0 then
      cons_arbre nc (insertion cp x g) d
      |> reequilibre
    else s

(* supprime le minimum d'un avl (retourne la valeur du minimum et l'arbre privé de ce minimum) *)
let rec suppression_min (s: 'a set): 'a * 'a set =
  match s with
  | V -> failwith "no min"
  | N(nc, V, d) -> nc.elem, d
  | N(nc, g, d) ->
    let min, gg = suppression_min g in
    min, (reequilibre (cons_arbre nc gg d))

(* supprime une clé d'un ensemble *)
let rec suppression (cp) (x: 'a) (s: 'a set) : 'a set  =
  match s with
  | V -> V
  | N({elem = y} as nc, g, d) -> 
    let c = cp x y in
    if c > 0 then
      cons_arbre nc g (suppression cp x d)
      |> reequilibre
    else if c < 0 then
      cons_arbre nc (suppression cp x g) d
      |> reequilibre
    else
      match d with
      | V -> g
      | _ ->
        let mind, dd = suppression_min d in
        cons_arbre {nc with elem = mind} g dd
        |> reequilibre

module Test =
struct
  type 'a tset = 'a list
  let rec mem cp (x: 'a) (s: 'a tset) =
    match s with
    | [] -> false
    | y :: ys ->
      let c = cp x y in
      (c = 0) || (c > 0 && (mem cp x ys))
  let insertion cp (x: 'a) (s: 'a tset) =
    let rec aux (prev: 'a list) (after: 'a list): 'a list = 
      match after with
      | [] -> List.rev_append prev [x]
      | y :: ys ->
        let c = cp x y in
        if c = 0 then s
        else if c > 0 then aux (y :: prev) ys
        else List.rev_append prev (x :: after)
    in
    aux [] s
  let suppression cp (x: 'a) (s: 'a tset) =
    let rec aux (prev: 'a list) (after: 'a list): 'a list = 
      match after with
      | [] -> s
      | y :: ys ->
        let c = cp x y in
        if c = 0 then List.rev_append prev ys
        else if c > 0 then aux (y :: prev) ys
        else s
    in
    aux [] s
  let print pp fmt x =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         pp
      ) x
end

let ex =
  N({hauteur = 2; elem = 6},
    N({hauteur = 1; elem = 3},
      N({hauteur = 0; elem = 1}, V, V),
      N({hauteur = 0; elem = 4}, V, V)
     ),
    N({hauteur = 0; elem = 8}, V, V)
   )

(* Génère un entier uniformément dans [|0, m-1|] *)
let gen_int (m: int) = Random.int m

(* Génère une liste de taille moyenne n, dont les entiers sont choisis
   uniformément dans [|0, m-1|] *)
let gen_list (n: int) (m: int) : int list =
  let rho = 1. /. (float_of_int n) in
  let rec aux () =
    if (Random.float 1. >= rho) then (gen_int m) :: (aux ())
    else []
  in aux ()

let rec fold_int f a b acc =
  if a = b then acc else fold_int f (a+1) b (f acc a)

exception Bad
(* vérifie les hauteurs et la propriété AVL*)
let check_heights (x: 'a set): bool =
  let rec aux (x: 'a set): int =
    match x with
    | V -> -1
    | N({hauteur = h}, g, d) ->
      let hg = aux g in
      let hd = aux d in
      let h_theorique = 1 + (max hg hd) in
      let delta = abs (hg - hd) in
      if h != h_theorique || delta > 1 then raise Bad
      else h_theorique
  in
  try let _ = aux x in true
  with
  | Bad -> false

(* vérifie que l'arbre est un ABR *)
let is_abr cp (x: 'a set): bool =
  let rec aux (cp) (x: 'a set) (binf: 'a option) (bsup: 'a option) : bool =
    match x with
    | V -> true
    | N({elem = elem}, g, d) ->
      ( match bsup with
        | Some(bsup) -> (cp elem bsup <= 0)
        | None -> true)
      &&
      ( match binf with
        | Some(binf) -> (cp binf elem <= 0)
        | None -> true
      )
      && aux cp g (binf) (Some(elem))
      && aux cp d (Some(elem)) (bsup)
  in
  aux cp x None None

(* vérifie que l'arbre est un avl *)
let is_avl (cp) (x: 'a set): bool =
  (is_abr cp x) && (check_heights x)

let gen_param_length = 100
let gen_param_ampl   = 100

let test_insertion () =
  let test_size = 1000 in
  for _ = 0 to test_size do
    let t = gen_list gen_param_length gen_param_ampl in
    let rep_stu =
      List.fold_left (fun acc x ->
        insertion (-) x acc 
      ) V t in
    let rep_cor =
      List.fold_left (fun acc x ->
          Test.insertion (-) x acc 
        ) [] t
    in
    if not (is_avl (-) rep_stu) then
      begin
        Format.printf "Échec de l'insertion --pas un AVL-- sur entrée : @. %a@."
          (Test.print Format.pp_print_int) t;
        assert false
      end;

    if ((elements rep_stu) <> rep_cor) then
      begin
        Format.printf "Échec de l'insertion --pas bons éléments-- sur entrée : @. %a@."
          (Test.print Format.pp_print_int) t;
        assert false
      end
  done

let test_suppression () =
  let test_size = 1000 in
  for _ = 0 to test_size do
    let tins = gen_list gen_param_length gen_param_ampl in
    let tsup = gen_list gen_param_length gen_param_ampl in
    let rep_stu =
      List.fold_left (fun acc x ->
          insertion (-) x acc 
        ) V tins in
    let rep_stu =
      List.fold_left (fun acc x ->
          suppression (-) x acc 
        ) rep_stu tsup in
    let rep_cor =
      List.fold_left (fun acc x ->
          Test.insertion (-) x acc 
        ) [] tins in
    let rep_cor =
      List.fold_left (fun acc x ->
          Test.suppression (-) x acc 
        ) rep_cor tsup in
    if not (is_avl (-) rep_stu) then
      begin
        Format.printf "Échec de la suppression --pas un AVL-- sur entrée : @. Insertion : %a@.Suppression : %a@."
          (Test.print Format.pp_print_int) tins
          (Test.print Format.pp_print_int) tsup;
        assert false
      end;

    if ((elements rep_stu) <> rep_cor) then
      begin
        Format.printf "Échec de la suppression --pas bons éléments-- sur entrée : @. Insertion : %a@.Suppression : %a@."
          (Test.print Format.pp_print_int) tins
          (Test.print Format.pp_print_int) tsup;
        assert false
      end
  done
