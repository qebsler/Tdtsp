open Graph.Gen
open Graph
open Graph.Tdtsp
open Graph.Dcgraph
(* open Graph.Sa *)
(* open Lib.Perm *)
(* NOTE: data pour tester -> http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/ *)

let () =
  Random.self_init ();

  let setup_plot (lpoint: (int*int) list) (circuit: int array): unit =
    for i = 0 to (Array.length circuit) - 2 do
      let xs, ys = List.nth lpoint circuit.(i) in
      let xf, yf = List.nth lpoint circuit.(i + 1) in
      Printf.printf "%d\t%d\t%d\t%d\n" xs ys xf yf
    done
  in

  let graph = lpoint_to_matrix Lpoints.test in
  let dgraph = cgraph_euclidian_dcgraph graph 20 in
  Printf.printf "OK";

  let tour_gl, val_gl = glouton_2 first dgraph in
  let tour_sa, val_sa = Sa.sa_settings_2 dgraph tour_gl in
  setup_plot Lpoints.test tour_sa;

  Printf.printf "\n%d\n" val_sa;
  Printf.printf "\n%d\n" val_gl;
