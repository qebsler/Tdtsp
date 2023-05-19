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
  let size = n |> float_of_int |> sqrt |> int_of_float in
  let p_size = size * size in
  let points = List.init n (fun _ -> (Random.int p_size, Random.int p_size)) in
  (* List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) points; *)
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
  Array.init size (fun i -> Array.init size (fun j -> fun t -> graph.(t).(i).(j)))

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
let random_euclidian_dcgraph (size: int) (varia: int): Dcgraph.t =
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
  let matrix_seed = random_dcgraph_seed size in
  let graph = random_euclidian_cgraph size in
  let f i j = fun t -> let v = (Utils.random_func2 varia matrix_seed.(i).(j) t) + graph.(i).(j) in
    if i = j || v < 0 then 0 else v
  in
  Array.init size (fun i -> Array.init size (f i))

(** L'appel à [random_euclidian_dcgraph_varia size] retourne une graphe euclidien-like dynamique à variation bornée par un nombre aléatoire *)
let random_euclidian_dcgraph_varia (size: int): Dcgraph.t =
  let r = min (Utils.isqrt size |> Random.int) 3 in
  random_euclidian_dcgraph size r

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
