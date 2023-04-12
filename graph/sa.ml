open Lib

(** Ce type permet de réunir les différents paramètres de l'algorithme de recuit simulé *)
type parameter = {t0: float; func: float -> float; tf: float; iter: float -> int}

(** Algorithme de recuit simulé appliqué au calcul d'un circuit de plus courte durée *)
let sim_annealing (settings: parameter) (graph: Dcgraph.t) (tour: Perm.t): Perm.t * int =
  let n = Dcgraph.size graph in
  let tour_opt = ref tour and value_opt = ref (Dcgraph.circuit_cost graph tour) in
  let file = open_out "sa/test_1.dat" in
  let rec aux (temp: float) (tour: Perm.t) (value: int): Perm.t * int =
    if temp <= settings.tf
    then (tour, value)
    else begin
      let tour' = ref tour and value' = ref (Dcgraph.circuit_cost graph tour) in
      Printf.fprintf file "%f\t%d\n" temp value;
      for _ = 0 to settings.iter temp do
        (* Printf.fprintf file "%d\n" !value_opt; *)
        let i = Random.int n and j = Random.int n in
        let tour_i = Perm.perm2 !tour' i j in
        let value_i = Dcgraph.circuit_cost graph tour_i in
        if value_i <= !value'
          then begin
          (* Printf.fprintf file "%d\n" value_i; *)
            if value_i <= !value_opt then begin
              tour_opt := tour_i;
              value_opt := value_i;
            end;
            tour' := tour_i;
            value' := value_i;
          end
        else begin
          let diff = (float_of_int (!value' - value_i)) in
          let r = Random.float 1. and prob = exp (diff /. temp) in
          if r <= prob then begin
            (* Printf.fprintf file "%d\n" value_i; *)
            tour' := tour_i;
            value' := value_i;
          end
        end
      done;
      let temp' = settings.func temp in
      aux temp' !tour' !value'
    end
  in
  let _, _ = aux settings.t0 tour (Dcgraph.circuit_cost graph tour) in
  (* close_out file; *)
  (!tour_opt, !value_opt)

(* Fonctionne très bien pour les graphes de taille 10 a 15 *)
let settings = {t0 = 20.; func = (fun i -> 0.95 *. i); tf = 0.2; iter = fun i -> int_of_float (40. *. i)}
let test = {t0 = 5.; tf = 0.5; func = (fun i -> 0.98 *. i); iter = (fun i -> int_of_float (50. *. i))}

let sa_default_settings = sim_annealing settings
let sa_test_settings = sim_annealing test

(** Test de l'algorithme de recuit simulé sur les paramètres [settings] sur des graphes de taille [size].
    On peut aussi régler le nombre de test qui seront effectués avec [nb_tets] *)
let bench (settings: parameter) (size: int) (nb_test: int): unit =
  let first (dgraph: Dcgraph.t): Cgraph.t =
    Array.map (Array.map (fun x -> x 0)) dgraph
  in
  for _ = 0 to nb_test - 1 do
    let graph = Dcgraph.random_dcgraph_2 size 10 in
    let tour_gl, value_gl = Tdtsp.glouton_2 graph first in
    let _, value_sa = sim_annealing settings graph tour_gl in
    Printf.printf "%d\t%d\n" value_gl value_sa;
  done

let bench_default = bench settings
let bench_test = bench test

(** Mesure la dispersion des résultats de l'algorithme de recuit simulé de paramètres [settings] sur un même graphe
    de taille [size]. Ecrit les données trouvées dans le fichier [filename] *)
let value_extension (settings: parameter) (graph: Dcgraph.t) (filename: string): float =
  let test_per_graph = 50 in
  let file = open_out filename in
  let sum = ref 0 in
  let circuit, cost = Tdtsp.glouton_2 graph Dcgraph.first in
  Printf.fprintf file "%s\n" (Tdtsp.sprintf_cost (circuit, cost));
  for _ = 1 to test_per_graph do
    let sol' = sim_annealing settings graph circuit in
    Printf.fprintf file "%s\n" (Tdtsp.sprintf_cost sol');
    let _, value = sol' in
    sum := !sum + value;
  done;
  close_out file;
  ((float_of_int !sum) /. (float_of_int test_per_graph))

let value_extension_default = value_extension settings


