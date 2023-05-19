(** Implementation de graphe complet non orienté à pondération > 0 *)
type t  = int array array

(** Renvoie le graphe vide *)
let empty (): t = [||]

(** Nombre de noeuds du graphe *)
let size (g: t): int = Array.length (g)

(** Créé un graphe complet à n noeuds avec les arêtes initialisées à 0 *)
let init (n: int): t = Array.make_matrix n n 0

(** Modifie la valeur d'une arete *)
let set_edge (g: t) (x: int) (y: int) (value: int): unit = g.(x).(y) <- value; g.(y).(x) <- value

(** Affiche le graphe *)
let print (g: t): unit =
  Array.iter (fun row -> Printf.printf "[ "; Array.iter (Printf.printf "%d\t") row; Printf.printf "]\n") g

let is_empty (g: t): bool = (g = [||])

(* let neighborhood (g: t) (n: int): int * int array = *)
(*   let size = Array.length g in *)
(*   Array.init size (fun i -> let x = if i < n then i else i + 1 in g.(n).(x)) *)

(** Calcule le sommet accessible de distance minimale *)
let min_next_vertex (g: t) (vertex: int): int =
  let aux (acc: int * int * int option) (x: int): int * int * int option =
    let ind_min, ind, min = acc in
    match min with
    | None  when x <> 0 -> (ind, ind + 1, Some x)
    | Some (a) when x <> 0 -> if x < a then (ind, ind + 1, Some x) else (ind_min, ind + 1, min)
    | _ -> (ind_min, ind + 1, min)
  in let ind, _, _ = Array.fold_left aux (0, 0, None) g.(vertex) in ind

(** Retire un sommet du graphe *)
let rem_vertex_graph (g: t) (n: int): t =
  let rem_vertex (ar: int array): int array =
    Array.init (Array.length ar - 1) (fun i -> if i < n then ar.(i) else ar.(i + 1))
  in Array.init (Array.length g - 1) (fun i -> if i < n then rem_vertex g.(i) else rem_vertex g.(i + 1))

(** Calcule la norme d'une matrice *)
let norm2 (g: t): int =
  Array.fold_left (fun acc t -> Array.fold_left (fun acc' x -> x * x + acc') acc t) 0 g
  |> float_of_int
  |> Float.sqrt
  |> int_of_float

let norm1 (g: t): int =
  Array.fold_left (fun acc t -> Array.fold_left (fun acc' x -> acc' + x) acc t) 0 g


type edge = {cost: int; i: int; j: int}

(** Type Union-Find *)
type uf_elem = {value: int; mutable parent: uf_elem option}
type uf = uf_elem array

(** Fonction pour le tri des aretes par poids croissant *)
let compare_edges (e: edge) (e': edge): int =
  e.cost - e'.cost

(** Transforme un graphe en liste d'aretes *)
let graph_to_edge_list (g: t): edge list =
  let l = ref [] and size = Array.length g in
  for i = 0 to size - 1 do
    for j = i + 1 to size - 1 do
      l := {cost = g.(i).(j); i = i; j = j} :: !l;
    done;
  done;
  !l

(** Retire une arete de la liste l *)
let rem_edges (l: edge list) (v: int): edge list =
  List.filter (fun x -> not (x.i = v || x.j = v)) l

(** Operation sur la structure UnionFind *)

(** Retourne le representant d'un element a *)
let rec find (a: uf_elem): uf_elem =
  match a.parent with
  | None -> a
  (* | Some p -> find p *)
  | Some p -> let parent = find p in a.parent <- Some parent; parent

(** Fusionne deux classes d'equivalence *)
let union (a: uf_elem) (b: uf_elem) =
  let pa = find a and pb = find b in
  pb.parent <- Some pa

(** Algorithme de Kruskal pour le calcul d'un ACPM dans un graphe sous forme de liste d'arete.
    Complexité: O(|S|^2) *)
let kruskal (g: t): edge list =
  let size = Array.length g in
  let edges = graph_to_edge_list g
    |> List.sort compare_edges in
  let partition = Array.init size (fun i -> {value = i; parent = None}) in
  let rec aux (edges_l: edge list) (acc: edge list) (n: int): edge list =
    match edges_l with
    | [] -> acc
    | _ when n <= 1 -> acc
    | h :: el when find partition.(h.i) = find partition.(h.j) -> aux el acc n
    | h :: el -> union partition.(h.i) partition.(h.j); aux el (h :: acc) (n - 1)
  in aux edges [] size

(** Retourne les successeurs de s dans le graphe el *)
let succ_g (el: edge list) (s: int): int list =
  List.filter (fun x -> x.i = s || x.j = s) el
    |> List.map (fun x -> if x.i = s then x.j else x.i)

(** Retourne un  parcours en profondeur de l'arbre el en partant du sommet s.
    Complexité: O(|S|) *)
let dfs (el: edge list) (s: int): int list =
  let visited = ref [s] in
  let rec aux (s: int): int list =
    (* List.iter (Printf.printf "%d ") (succ_g el s); print_newline (); *)
    let l = List.filter (fun x -> not (List.mem x !visited)) (succ_g el s)
      |> List.map (fun x -> visited := x :: !visited; aux x)
      |> List.flatten
    in s :: l
  in aux s

(** Retourne tout les tours générés par un parcours en profondeur de l'ACPM du graphe g.
    Remarque: c'est une 2-approx dans le cas métrique.
    Complexité: O(|S|^2) *)
let tsp (g: t) (i: int): int list =
  let acpm = kruskal g in
  dfs acpm i

let acpm_cost (g: t): int =
  kruskal g
  |> List.fold_left (fun acc x -> acc + x.cost) 0
