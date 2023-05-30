open Lib

(** Regroupe l'ensemble des fonctions de générations de graphes complet (dynamique ou statique) *)

type point = int * int

(** Calcul de la distance euclidienne (partie entière) *)
let fl_eucl_dist (a: point) (b: point): int =
  let x1, y1 = a and x2, y2 = b in
  let dx = x1 - x2 and dy = y1 - y2 in
  (Float.pow (float_of_int dx) 2.) +. (Float.pow (float_of_int dy) 2.)
  |> Float.sqrt
  |> int_of_float

(** Converti une liste de point en un graphe complet.
    Complexité: O(n^2) *)
let lpoint_to_matrix (l: point list): Cgraph.t =
  let size = List.length l in
  let m = Array.make_matrix size size 0 in
  let rec aux (lp: point list) (j: int): unit =
    match lp with
    | [] -> ()
    | p :: lp' ->
        List.iteri (fun i x -> let d = fl_eucl_dist p x in
          m.(j).(i+j+1) <- d;
          m.(i+j+1).(j) <- d) lp';
        aux lp' (j+1);
  in aux l 0; m

(** Génère un graphe euclidien complet aléatoire *)
let random_euclidian_cgraph (n: int): Cgraph.t =
  let p_size = n * n in
  let points = List.init n (fun _ -> (Random.int p_size, Random.int p_size)) in
  (* List.iter (fun (x, y) -> Printf.printf "(%d, %d);\n" x y) points; *)
  (* Printf.printf "\n"; *)
  lpoint_to_matrix points

let random_cgraph (n: int): Cgraph.t =
  let matrix = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let r = Random.int 20 in
      matrix.(i).(j) <- r;
      matrix.(i).(j) <- r;
    done;
  done;
  matrix


(* Génération de dcgraph *)

(** Génère un graphe aléatoire complet dynamique *)
let random_dcgraph (size: int) (period: int): Dcgraph.t =
  let graph = Array.init period (fun _ -> random_cgraph size) in
  Array.init size (fun i -> Array.init size (fun j -> fun t -> graph.(t mod period).(i).(j)))

(** Retourne un graphe aléatoire basé sur un graphe euclidien *)
let random_euclidian_dcgraph_2 (size: int) (varia: int): Dcgraph.t =
  (* Génère les seed pour les pondérations du dcgraph *)
  let random_dcgraph_seed (n: int): float array array =
    let mat = Array.make_matrix n n 0. in
    for i = 0 to n - 1 do
      for j = 0 to i - 1 do
        let r = Random.float 1. in
        mat.(i).(j) <- r;
        mat.(j).(i) <- r;
      done;
    done;
    mat
  in
  let matrix_seed = random_dcgraph_seed size in
  let graph = random_euclidian_cgraph size in
  let f i j = fun t -> let v = (Utils.random_func varia matrix_seed.(i).(j) t) + graph.(i).(j) in
    if i = j || v < 0 then 0 else v
  in
  Array.init size (fun i -> Array.init size (f i))

(** Retourne un graphe euclidien dynamique à variation bornée *)
let cgraph_euclidian_dcgraph (graph: Cgraph.t) (varia: int): Dcgraph.t =
  (* Génère les seed pour les fonctions de pondérations du dcgraph *)
  let random_dcgraph_seed (n: int): (int * int) array array =
    let mat = Array.make_matrix n n (0, 0) in
    for i = 0 to n - 1 do
      for j = 0 to i - 1 do
        let s = (Random.int 10107, Random.int 11893) in
        mat.(i).(j) <- s;
        mat.(j).(i) <- s;
      done;
    done;
    mat
  in
  let size = Array.length graph in
  let matrix_seed = random_dcgraph_seed size in
  let f i j = fun t -> let v = (Utils.random_func2 varia matrix_seed.(i).(j) t) + graph.(i).(j) in
    if i = j || v < 0 then 0 else v
  in
  Array.init size (fun i -> Array.init size (f i))

(** L'appel à [random_euclidian_dcgraph_varia size] retourne une graphe euclidien-like dynamique à variation bornée par un nombre aléatoire *)
(* let random_euclidian_dcgraph_varia (size: int): Dcgraph.t = *)
(*   let r = max (Utils.isqrt size |> Random.int) 3 in *)
(*   random_euclidian_dcgraph size r *)

let cgraph_to_dcgraph_square (graph: Cgraph.t) (period: int) (ampl: int array array) (delay: int): Dcgraph.t =
  let size = Cgraph.size graph in
  let sq = Utils.square_signal period delay in
  let f i j = fun t -> graph.(i).(j) + (sq t) * ampl.(i).(j) in
  Array.init size (fun i -> Array.init size (f i))

(** Transforme un cgraph en dcgraph en ajoutant un signal carré sur les pondérations *)
let cgraph_to_dcgraph_with_random_square (graph: Cgraph.t): Dcgraph.t =
  let size = Cgraph.size graph in
  let ampl = Utils.random_sym_matrix size 4 in
  let period = 100 in
  let delay = Random.int period in
  cgraph_to_dcgraph_square graph period ampl delay

(** Transforme un cgraph en dcgraph en ajoutant {math f \times ampl} sur les pondérations initiale.
    On peut controler les régions ciblés en mettant l'amplitude à 0 si le graphe dynamique ne doit pas différer dun graphe statique initial. *)
let cgraph_to_dcgraph (f: int -> int) (ampl: int array array) (graph: Cgraph.t): Dcgraph.t =
  let size = Cgraph.size graph in
  let f i j = fun t -> graph.(i).(j) + ampl.(i).(j) * (f t) in
  Array.init size (fun i -> Array.init size (f i))

(** Retourne un dcgraph aléatoire dont les pondérations ne varie pas *)
let random_static_dcgraph (size: int): Dcgraph.t =
  let cgraph = random_cgraph size in
  let ampl = Array.make_matrix size size 0 in
  let fn = fun i -> i in
  cgraph_to_dcgraph fn ampl cgraph

(** Transforme un graphe statique en un graphe dynamique par ajout d'une variation sous forme d'un echelon periodique *)
let cgraph_to_dcgraph_echelon (period: int) (duration: int) (delay: int) (ampl: int array array) (graph: Cgraph.t): Dcgraph.t =
  let f = Utils.echelon period duration delay in
  cgraph_to_dcgraph f ampl graph

(** Transforme un graphe statique en un graphe dynamique par l'ajout d'un echelon aléatoire de période 100 *)
let cgraph_to_dcgraph_with_random_echelon (graph: Cgraph.t): Dcgraph.t =
  let size = Cgraph.size graph in
  let period = 100 in
  let duration = Random.int period in
  let delay = Random.int period in
  let ampl = Utils.random_sym_matrix size 4 in
  cgraph_to_dcgraph_echelon period duration delay ampl graph

(** Retourne une matrice à coefficients dans {0, 1} symétrique tels que s'il y a un 1 dans une case alors la colonne contient
    uniquement des 1 ou bien la ligne ne contient que des 1 *)
let random_region (size: int): int array array =
  let fold (acc: int list) (x: int option): int list =
    match x with
    | None -> acc
    | Some i -> i :: acc
  in
  let update_row (matrix: int array array) (row: int): unit =
    for j = 0 to size - 1 do
      if j <> row then matrix.(row).(j) <- 1;
    done
  in
  let update_column (matrix: int array array) (col: int): unit =
    for i = 0 to size - 1 do
      if i <> col then matrix.(i).(col) <- 1;
    done
  in
  let region_with_one = List.init size (fun i -> if Random.bool () then Some i else None)
    |> List.fold_left fold []
  in
  let matrix = Array.make_matrix size size 0 in
  List.iter (fun x -> update_column matrix x; update_row matrix x) region_with_one;
  matrix

exception Different_size

(** Retourne le produit coordonnée par coordonnée de deux matrices de meme taille *)
let ( ** ) (m: int array array) (m': int array array): int array array =
  let n = if Array.length m = Array.length m then Array.length m else raise Different_size in
  Array.init n (fun i -> Array.init n (fun j -> m.(i).(j) * m'.(i).(j)))

(** Retourne le produit d'une matrice de region aléatoire par une matrice d'amplitude alétoire *)
let random_region_ampl (size: int): int array array =
  let region = random_region size in
  let ampl = Utils.random_sym_matrix size 6 in
  region ** ampl

(** Retourne un dcgraph basé sur un graphe euclidien, dont les variations de pondérations sont sous forme d'échelon *)
let random_euclidian_dcgraph_echelon_region (size: int): Dcgraph.t =
  let euclidian_graph = random_euclidian_cgraph size in
  let ampl = random_region_ampl size in
  let period = 100 in
  let echelon = Utils.random_echelon period in
  cgraph_to_dcgraph echelon ampl euclidian_graph

(** Retourne un graphe aléatoire dont les variations sont sous formes d'échelon *)
let random_dcgraph_echelon_region (size: int): Dcgraph.t =
  let graph = random_cgraph size in
  let ampl = random_region_ampl size in
  let period = 100 in
  let echelon = Utils.random_echelon period in
  cgraph_to_dcgraph echelon ampl graph
