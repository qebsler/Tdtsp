open Lib

(** Implementation de graphes dynamique via des matrices de suites *)
type t = (int -> int) array array

let empty (): t = [||]

(** Retourne le nombre de sommets dans le graphe *)
let size (dg: t): int = Array.length dg

(** Affiche les n premiers graphes *)
let print (dg: t) (n: int): unit =
  let size = Array.length dg in
  for t = 0 to n - 1 do
    for i = 0 to size - 1 do
      Printf.printf "[";
      Array.iter (fun x -> Printf.printf "%d\t" (x t)) dg.(i);
      Printf.printf "]\n";
    done;
    Printf.printf "\n";
  done

let cgraph_at (graph: t) (t: int): Cgraph.t =
  let n = size graph in
  Array.init n (fun i -> Array.init n (fun j -> graph.(i).(j) t))

(** Transforme un graphe en graphe dynamique *)
let cgraph_to_dcgraph (graph: Cgraph.t): t =
  let n = Array.length graph in
  Array.init n (fun i -> Array.init n (fun j -> fun _ -> graph.(i).(j)))

(** Calcul de la durée d'un chemin quelconque.
    Complexité: O(n) *)
let chemin_cost (dg: t) (vertex_t: int array): int =
  let rec aux (i: int) (acc: int): int =
    match i with
    | i when i + 1 = Array.length vertex_t -> acc
    | i -> let next = vertex_t.(i + 1) and current = vertex_t.(i) in
        let acc' = (dg.(current).(next) acc) + acc in
        aux (i + 1) acc'
  in aux 0 0

(** Calcul de la durée d'un circuit en utilisant la durée d'un chemin.
    Complexité: O(|S|) *)
let circuit_cost (dg: t) (tour: Perm.t): int =
  let chemin = chemin_cost dg tour in
  let l = Array.length tour in
  let finish = tour.(l - 1) and start = tour.(0) in
  let corr = dg.(finish).(start) chemin in
  chemin + corr

(** Génère un graphe complet dynamique au hasard *)
let random_dcgraph (size: int) (period: int): t =
  let graph = Array.init period (fun _ -> Cgraph.random_cgraph size) in
  Array.init size (fun i -> Array.init size (fun j -> fun t -> graph.(t).(i).(j)))

(** Calcule le graphe moyen d'un graphe dynamique.
    Complexité: O(|S|^2 * p) *)
let mean_graph (period: int) (dg: t): Cgraph.t =
  let size = Array.length dg in
  let sum_graph = Array.make_matrix size size 0 in
  for k = 0 to period - 1 do
    Array.iteri (fun i -> Array.iteri (fun j f -> sum_graph.(i).(j) <- sum_graph.(i).(j) + (f k))) dg
  done;
  Array.map (Array.map (fun x -> x / period)) sum_graph

(** Genere une matrice de seed pour un dcgraph aleatoire *)
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

let random_dcgraph_seed2 (n: int): (int * int) array array =
  let mat = Array.make_matrix n n (0, 0) in
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      let s = (Random.int 10120, Random.int 11900) in
      mat.(i).(j) <- s;
      mat.(j).(i) <- s;
    done;
  done;
  mat

(** Retourne un graphe aléatoire basé sur un graphe euclidien *)
let random_dcgraph_2 (size: int) (varia: int): t =
  let matrix_seed = random_dcgraph_seed size in
  let graph = Cgraph.random_cgraph size in
  let f i j = fun t -> let v = (Utils.random_func varia matrix_seed.(i).(j) t) + graph.(i).(j) in
    if i = j || v < 0 then 0 else v
  in
  Array.init size (fun i -> Array.init size (f i))

let random_dcgraph_3 (size: int) (varia: int): t =
  let matrix_seed = random_dcgraph_seed2 size in
  let graph = Cgraph.random_cgraph size in
  let f i j = fun t -> let v = (Utils.random_func2 varia matrix_seed.(i).(j) t) + graph.(i).(j) in
    if i = j || v < 0 then 0 else v
  in
  Array.init size (fun i -> Array.init size (f i))

(* TODO: Tester *)
let time_at (dg: t) (tour: Perm.t) (v: int) =
  let exception Break of int in
  let pos = try Array.fold_left (fun acc x -> if x = v then raise (Break acc) else acc + 1) 0 tour with
    | Break n -> n
  in
  let path_to_v = Array.init (pos + 1) (fun i -> tour.(i)) in
  chemin_cost dg path_to_v

(* Retourne les variations de la solution sol par des 2perm *)
(* let opt2 (dgraph: t) (sol: Perm.t * int): (Perm.t * int) array = *)
(*   let circuit, cost = sol in *)
(*   let arr = Perm.opt2_shuffle circuit *)
(*     |> Array.map (fun x -> (x, circuit_cost dgraph x)) *)
(*   in Array.sort (fun a b -> let _, c1 = a and _, c2 = b in c1 - c2) arr; *)
(*   arr *)

(** Retourne le graphe à l'instant 0 *)
let first (g: t): Cgraph.t =
  Array.map (Array.map (fun x -> x 0)) g
