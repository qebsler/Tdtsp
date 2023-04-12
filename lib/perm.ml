(** Implementation de permutation de [0; n - 1] *)
type t = int array

(** Vérifie qu'un tableau d'entiers est bien une permutation *)
let is_perm (tab: t): bool =
  let size = Array.length tab in
  let count = Array.make size 0 in
  try Array.iter (fun x -> count.(x) <- (count.(x) + 1)) tab; count = (Array.make size 1) with
    Invalid_argument (_) -> false

exception Not_Perm

(** Modifie le tableau via une transposition *)
let transpose (tab: t) (i: int) (j: int): unit =
  let tmp = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- tmp

(** Retourne la permutation identité *)
let perm_id (n: int): t = Array.init n (fun i -> i)

(** Retourne la longueur d'une permutation *)
let size (tab: t): int = Array.length tab

(** Retourne la transposition qui echange i et j *)
let transposition (n: int) (i: int) (j: int): t =
  let p = perm_id n in
  transpose p i j; p

(** Affiche la permutation *)
let print (tab: t): unit =
  print_string "( "; Array.iter (Printf.printf "%d ") tab; print_char ')';
  print_newline ()

(** Calcule la composée de deux permutations : tab ° tab' *)
exception Incompatible_perm
let compose (tab: t) (tab': t): t =
  let size = Array.length tab and size' = Array.length tab' in
  if is_perm tab && is_perm tab' then begin
    if size = size' then Array.init size (fun i -> tab.(tab'.(i))) else raise Incompatible_perm
  end else raise Not_Perm

(** Calcule l'inverse de la permutation tab *)
let inverse (tab: t): t =
  if is_perm tab then begin
    let tab' = Array.make (Array.length tab) 0 in
    Array.iteri (fun i x -> tab'.(x) <- i) tab; tab'
  end else raise Not_Perm

(** Génère toutes les permutation de [0; n - 1]  via l'algorithme de Heap.*)
let all_perm (n: int): t list =
  let perms = ref [] in
  (* Algorithme de Heap *)
  let rec heap (k: int) (tab: t): unit =
    match k with
    | 1 -> perms := (Array.copy tab) :: !perms
    | k ->
        heap (k - 1) tab;
        for i = 0 to k - 2 do
          if k mod 2 = 0 then transpose tab i (k - 1) else transpose tab 0 (k - 1);
          heap (k - 1) tab
        done;
  in heap n (perm_id n); !perms

(** Genere une permutation aléatoirement grâce à l'algorithme de Fisher-Yates *)
let random (n: int): t =
  let r = perm_id n in
  for i = n - 1 downto 1 do
    let j = Random.int i in
    transpose r i j
  done;
  r

(** Retourne une transposition choisie au hasard *)
let random_transposition (n: int): t =
  let i = Random.int n in
  let tmp = Random.int (n - 1) in
  let j = if tmp >= i then tmp + 1 else tmp in
  let tr = perm_id n in
  transpose tr i j;
  tr

(** Retourne le sous-tableau allant de l'indice i à j (inclus) *)
let perm2 (tab: t) (i: int) (j: int): t =
  let n = Array.length tab in
  let m = min i j and ma = max i j in
  let init (k: int): int =
    match k with
    | k when k < m || k > ma -> tab.(k)
    | k -> tab.(ma - k + m)
  in
  Array.init n init

