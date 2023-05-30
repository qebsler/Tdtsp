open Lib

(** Genère une population de solutions à partir d'une solution intiale *)
let initial_population (graph: Dcgraph.t) (size: int) (seed: Perm.t * int): (Perm.t * int) list =
  let tour, _ = seed in
  let n = Perm.size tour in
  seed :: (List.init (size - 1)
    (fun _ -> let i = Random.int n and j = Random.int n in
              let tour' = Perm.perm2 tour i j in
              (tour', Dcgraph.circuit_cost graph tour')))

(** Selectionne les meilleures solutions dans la population de solutions [population].
    Retourne une population dont la taille est égal à [List.length population / factor]*)
let select (population: (Perm.t * int) list) (factor: int): (Perm.t * int) list =
  let cmp = fun a b -> let _, v1 = a and _, v2 = b in compare v1 v2 in
  let sorted = List.sort cmp population
  in
  let rec sub (i: int) (l: (Perm.t * int) list): (Perm.t * int) list =
    match l with
    | [] -> []
    | _ when i = 0 -> []
    | s :: l' -> s :: (sub (i - 1) l')
  in
  sub (List.length population / factor) sorted

(** Retourne une nouvelle population générée à partir d'une population de solutions, en conservant les solutions
    de la population initiale *)
let breed (graph: Dcgraph.t) (population: (Perm.t * int) list) (factor: int): (Perm.t * int) list =
  let gen (sol: Perm.t * int): (Perm.t * int) list =
    let tour, _ = sol in
    let size = Perm.size tour in
    sol :: (List.init (factor - 1) (fun _ -> let i = Random.int size and j = Random.int size in
                               let tour' = Perm.perm2 tour i j in
                               let value' = Dcgraph.circuit_cost graph tour' in
                               (tour', value')))
  in
  List.concat_map gen population

(** Retourne la population finale après [deepth] génération (selection des meilleurs puis reproduction à partir d'eux) *)
let rec generation (graph: Dcgraph.t) (deepth: int) (population: (Perm.t * int) list): (Perm.t * int) list =
  let factor = 5 in
  match deepth with
  | 0 -> population
  | i -> let selected = select population factor in
         let population' = breed graph selected factor in
         generation graph (i - 1) population'

