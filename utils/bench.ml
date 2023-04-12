open Graph
open Lib

(** Fonction générale de bench de la fonction [f] sur des entrées de taille [n]
    générées par la fonction [gen]. Retourne une chaine de caractère contenant
    le temps d'execution moyen de [f] sur les [nb_test] entrées multipliés par
    [factor] (pour limiter la perte de précision dû au printf) *)
let bench (factor: float) (n: int) (gen: int -> 'a) (f: 'a -> 'b): string =
  let nb_test = 50 in
  let sum_t = ref 0. in
  for _ = 1 to nb_test do
    let v = gen n in
    let t1 = Unix.gettimeofday () in
    let _ = f v in
    let t2 = Unix.gettimeofday () in
    sum_t := !sum_t +. (t2 -. t1);
  done;
  let time = !sum_t *. factor /. (float_of_int nb_test) in
  Printf.sprintf "%d\t%f\n" n time

(** Fonction générale pour comparer des fonctions sur des entrées identiques.
    L'appel [compare n gen [f1, f2, .., fk]] retourne les valeurs des fonctions [f1, .., fk] sur une entrée de taille n
    générées par la fonction [gen] *)
let compare (n: int) (gen: int -> 'a) (fl: ('a -> int) list): string =
  let nb_test = 100 in
  let single_test (_: int): string =
    let v = gen n in
    let res_l = List.map (fun f -> Printf.sprintf "%d" (f v)) fl in
    String.concat "\t" res_l
  in
  let res_s = List.init nb_test single_test in
  String.concat "\n" res_s

(** Retourne le temps moyen d'execution de la fonction [Dcgraph.circuit_cost] sur des entrées de taille [n] *)
let bench_circuit_cost (n: int): string =
  let gen (n: int): Dcgraph.t * Perm.t =
    let graph = Dcgraph.random_dcgraph_3 n (3 * n) in
    let tour = Perm.random n in
    graph, tour
  in
  let f (arg: Dcgraph.t * Perm.t): int =
    let graph, tour = arg in
    Dcgraph.circuit_cost graph tour
  in
  bench 100. n gen f

let bench_evo (n: int): string =
  let gen (n: int): Dcgraph.t * Tdtsp.solution =
    let graph = Dcgraph.random_dcgraph_3 n 15 in
    let sol = Tdtsp.glouton_2 graph Dcgraph.first in
    graph, sol
  in
  let f (arg: Dcgraph.t * Tdtsp.solution) =
    let graph, sol = arg in
    Tdtsp.evolutionnary graph 50 sol
  in
  bench 1. n gen f
