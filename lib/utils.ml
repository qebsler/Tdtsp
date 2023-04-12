(* Calcul de la factorielle de n (récursif terminale) *)
let fact (n: int): int =
  let rec aux (n: int) (acc: int): int =
    match n with
    | 1 | 0 -> acc
    | n     -> aux (n - 1) (acc * n)
  in aux n 1

(* Bijection de [1; n!] dans [0, 0]*...*[0, n-2]*[0, n-1] *)
let factoradic (n: int): int list =
  let rec aux (n: int) (i: int): int list =
    match n with
    | n when n < i -> [n]
    | n -> let n' = n / i in
      let r = n mod i in
      r :: (aux n' (i+1))
  in aux n 1

(* Réciproque de factoradic *)
let fact_to_dec (n: int list): int =
  let sum, _, _ = List.fold_left
    (fun acc k -> let sum, i, fi = acc in (sum + k * fi, i + 1, (i + 1) * fi))
    (0, 0, 1)
    n
  in sum

(* Bijection de [0, 0]*...*[0,n-2]*[0,n-1] vers S_(sn) *)
(* TODO: Refactor -> remove List.nth *)
let lehmer (sn: int) (n: int list): int array =
  let size = List.length n in
  let perm = Array.init sn (fun i -> i) in
  for i = sn - 1 downto 1 do
    let j = if i < size then List.nth n i else 0 in
    let tmp = perm.(i) in
    perm.(i) <- perm.(j);
    perm.(j) <- tmp;
  done;
  perm

let int_to_perm (n: int) (k: int): int array =
  factoradic k
    |> lehmer n

let argmin (t: int array): int =
  let fold_aux (acc: int option * int * int) (x: int) =
    let min, amin, i = acc in
    match min with
    | None -> (Some x, i, i + 1)
    | Some a -> if a < x then (Some a, amin, i + 1) else (Some x, i, i + 1)
  in let _, amin, _ = Array.fold_left fold_aux (None, -1, 0) t in
  amin

(* Genere une fonction aleatoire a valeurs dans [|0, size - 1|] *)
let random_func (size: int) (seed: float): int -> int =
  let m = float_of_int size in
  fun i -> let i' = float_of_int i in
    m *. sin (i' *. seed)
      |> int_of_float

let random_func2 (size: int) (seed: int * int): int -> int =
  let a, b = seed in
  fun x -> ((a * x + b) mod 1264411) mod size

let gen_2rand (n: int): int * int =
  let i = Random.int n in
  let j = ref (Random.int n) in
  while !j = i do
    j := Random.int n;
  done;
  (i, !j)
