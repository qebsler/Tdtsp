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


(** Calcule le graphe moyen d'un graphe dynamique.
    Complexité: O(|S|^2 * p) *)
let mean_graph (period: int) (dg: t): Cgraph.t =
  let size = Array.length dg in
  let sum_graph = Array.make_matrix size size 0 in
  for k = 0 to period - 1 do
    Array.iteri (fun i -> Array.iteri (fun j f -> sum_graph.(i).(j) <- sum_graph.(i).(j) + (f k))) dg
  done;
  Array.map (Array.map (fun x -> x / period)) sum_graph

let min_graph (period: int) (dg: t): Cgraph.t =
  let n = size dg in
  let rec min_fun (f: int -> int) (n: int) (acc: int): int =
    match n with
    | 0 -> acc
    | n when f n >= acc -> min_fun f (n - 1) acc
    | n -> min_fun f (n - 1) (f n)
  in Array.init n (fun i -> Array.init n (fun j -> min_fun dg.(i).(j) period (dg.(i).(j) 0)))

let max_graph (period: int) (graph: t): Cgraph.t =
  let n = size graph in
  let rec max_fun (f: int -> int) (n: int) (acc: int) =
    match n with
    | 0 -> acc
    | n when f n <= acc -> max_fun f (n - 1) acc
    | n -> max_fun f (n - 1) (f n)
  in
  Array.init n (fun i -> Array.init n (fun j -> max_fun graph.(i).(j) period (graph.(i).(j) 0)))

(* TODO: Tester *)
let time_at (dg: t) (tour: Perm.t) (v: int) =
  let exception Break of int in
  let pos = try Array.fold_left (fun acc x -> if x = v then raise (Break acc) else acc + 1) 0 tour with
    | Break n -> n
  in
  let path_to_v = Array.init (pos + 1) (fun i -> tour.(i)) in
  chemin_cost dg path_to_v

(** Retourne le graphe à l'instant 0 *)
let first (g: t): Cgraph.t =
  Array.map (Array.map (fun x -> x 0)) g

(** L'appel à [borne_inf graph interval] retourne une borne inférieure du tour hamiltonien en calculant
    l'arbre couvrant de poids minimal du graphe de plus faible pondération sur l'interval de temps [interval].
    Complexité en O(|S|^2 * interval) *)
let borne_inf (g: t) (interval: int): int =
  min_graph interval g
  |> Cgraph.acpm_cost
