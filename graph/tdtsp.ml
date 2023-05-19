open Lib

(** Type réunissant le tour et son cout *)
type solution = Perm.t * int

(** Affiche une solution avec son cout *)
let print (sol: solution): unit =
  let circuit, cost = sol in
  Printf.printf "Longueur: %d\n" cost;
  Perm.print circuit

(** Retourne la chaine de caractère contenant le coût d'une solution *)
let sprintf_cost (sol: solution): string =
  let _, cost = sol in
  Printf.sprintf "%d" cost;

(** Résoud le DTSP en générant S_n puis en le parcourant.
    Complexité: O(|S|*|S|!) *)
exception Empty_graph
let naif_1 (dg: Dcgraph.t): int =
  let min_cost (acc: int option) (perm: Perm.t): int option =
    match acc with
    | None     -> Some (Dcgraph.circuit_cost dg perm)
    | Some (n) -> let cost = Dcgraph.circuit_cost dg perm in
        if cost < n then Some (cost) else Some (n)
  in let opt = Array.length dg
    |> Perm.all_perm
    |> List.fold_left min_cost None
  in match opt with
    | None     -> raise Empty_graph
    | Some (n) -> n


(** Résoud le TdTSP en parcourant S_n à la volée.
    Complexité: O(|S|*|S|!) *)
let naif_2 (dg: Dcgraph.t): solution =
  let size = Array.length dg in
  let to_perm = Utils.int_to_perm size in
  let min_v = ref (Dcgraph.circuit_cost dg (to_perm 0)) in
  let min_perm = ref (to_perm 0) in
  for i = 1 to Utils.fact size do
    let perm = to_perm i in
    let value = Dcgraph.circuit_cost dg perm in
    if value < !min_v then begin
      min_v := value;
      min_perm := perm;
    end
  done;
  (* Perm.print !min_perm; *)
  (!min_perm, !min_v)

(** Résoud le TdTSP en moyennant le graphe dynamique puis en calculant un ACPM
    et retourne un parcours en profondeur de ACPM (2-approx dans le cas metrique).
    Complexité: O(|S|^2 * p) *)
let glouton_1 (dg: Dcgraph.t) (repr: Dcgraph.t -> Cgraph.t): solution =
  let t = repr dg
    |> Cgraph.kruskal
  in let circuit = Cgraph.dfs t 0
    |> Array.of_list
  in (circuit, Dcgraph.circuit_cost dg circuit)

(** Amelioration du précédent au depend d'une complexite plus elevé *)
let glouton_2 (dg: Dcgraph.t) (repr: Dcgraph.t -> Cgraph.t): solution =
  let aux (acc: (Perm.t * int) option) (x: Perm.t * int): (Perm.t * int) option =
    let _, l = x in
    match acc with
    | None -> Some x
    | Some (_, a) -> if l < a then Some x else acc
  in let graph_repr = repr dg in
  let v_opt = List.init (Dcgraph.size dg) (Cgraph.tsp graph_repr)
    |> List.map (fun l -> (Array.of_list l, Dcgraph.circuit_cost dg (Array.of_list l)))
    |> List.fold_left aux None
  in match v_opt with
    | None -> raise Empty_graph
    | Some x -> x

(** Amelioration de la fonction [glouton_2] grâce à du multithread. *)
let glouton_2_parallele (dg: Dcgraph.t) (repr: Dcgraph.t -> Cgraph.t): solution =
  let n = Dcgraph.size dg in
  let opt = ref None in
  let m = Mutex.create () in
  let graph_repr = repr dg in
  let acpm = Cgraph.kruskal graph_repr in
  let th_func (i: int): unit =
    let tour = Cgraph.dfs acpm i
      |> Array.of_list
    in let cost = Dcgraph.circuit_cost dg tour in
    Mutex.lock m;
    begin match !opt with
    | None -> opt := Some (tour, Dcgraph.circuit_cost dg tour)
    | Some (_, a) when cost < a -> opt := Some (tour, cost)
    | Some _ -> ()
    end;
    Mutex.unlock m
  in
  let th = List.init n (Thread.create th_func) in
  List.iter (Thread.join) th;
  match !opt with
  | None -> raise Empty_graph
  | Some x -> x

(** Tentative d'approximation de la solution optimale par une méthode inspirée de la selection naturelle.
    Complexité en O(n^2) *)
let evolutionnary (graph: Dcgraph.t) (gen: int) (seed: solution): solution =
  let size = Dcgraph.size graph in
  let pop_size = (5 * size * (size |> float_of_int |> log |> int_of_float)) in
  let pop = Evolution.initial_population graph pop_size seed in
  let final_pop = Evolution.generation graph gen pop in
  List.hd final_pop

(** Retourne un chemin au hasard.
    Complexité: O(1) *)
let random_1 (dg: Dcgraph.t): solution =
  let size = Array.length dg in
  let circuit = Perm.random size in
  (circuit, Dcgraph.circuit_cost dg circuit)
