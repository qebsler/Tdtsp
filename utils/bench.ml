(* open Graph *)
(* open Lib *)

(** Fonction générale de bench de la fonction [f] sur des entrées de taille [n]
    générées par la fonction [gen]. Retourne une chaine de caractère contenant
    le temps d'execution moyen de [f] sur les [nb_test] entrées multipliés par
    [factor] (pour limiter la perte de précision dû au printf) *)
let bench (factor: float) (n: int) (gen: int -> 'a) (f: 'a -> 'b): string =
  let nb_test = 250 in
  let sum_t = ref 0. in
  for _ = 1 to nb_test do
    let v = gen n in
    let t1 = Unix.gettimeofday () in
    let _ = f v in
    let t2 = Unix.gettimeofday () in
    sum_t := !sum_t +. (t2 -. t1);
  done;
  let time = !sum_t *. factor /. (float_of_int nb_test) in
  Printf.sprintf "%f" time

(** Fonction générale pour comparer des fonctions sur des entrées identiques.
    L'appel [compare n gen [f1, f2, .., fk]] retourne les valeurs des fonctions [f1, .., fk] sur une entrée de taille n
    générées par la fonction [gen] *)
let compare (n: int) (gen: int -> 'a) (fn_list: ('a -> int) list): string =
  let nb_test = 1000 in
  let single_test (_: int): string =
    let v = gen n in
    let res_l = List.map (fun f -> Printf.sprintf "%d" (f v)) fn_list in
    String.concat "\t" res_l
  in
  let res_s = List.init nb_test single_test in
  String.concat "\n" res_s

(** Bench toutes les fonctions de la liste fournie en argument *)
let multiple_bench (n: int) (gen: int -> 'a) (fn_list: ('a -> 'b) list) (factor_list: float list): string =
  (string_of_int n) :: (List.map2 (fun fn factor -> bench factor n gen fn) fn_list factor_list)
  |> String.concat "\t"
