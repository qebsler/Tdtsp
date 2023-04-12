open Graph
open Utils
(* open Utils.Bench *)
(* open Utils *)
(* NOTE: data pour tester -> http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/ *)

let () =
  Random.self_init ();
  (* let graph = Dcgraph.random_dcgraph_2 15 5 in *)
  (* let tour_gl, value_gl = Tdtsp.glouton_2 graph (Dcgraph.first) in *)
  (* let _, value_sa2 = Sa.sa_default_settings graph tour_gl in *)
  (* let _, value_sa1 = Sa.sa_test_settings graph tour_gl in *)
  (* let _, value_evo = Tdtsp.evolutionnary graph 50 (tour_gl, value_gl) in *)
  (* let norm = Dcgraph.cgraph_at graph 0 |> Cgraph.norm2 in *)

  (* (1* Dcgraph.print graph 1; *1) *)
  (* Printf.printf "Norme 2:\t%d\n" norm; *)
  (* Printf.printf "Glouton:\t%d\n" value_gl; *)
  (* Printf.printf "SAt:\t%d\n" value_sa1; *)
  (* Printf.printf "SAd:\t%d\n" value_sa2; *)
  (* Printf.printf "Evo:\t%d\n" value_evo; *)

  let gl = fun (graph, _) -> let _, v = Tdtsp.glouton_2 graph Dcgraph.first in v in
  let evo = fun (graph, sol) -> let _, v = Tdtsp.evolutionnary graph 50 sol in v in
  let sa1 = fun (graph, sol) -> let tour, _ = sol in let _, v =Sa.sa_test_settings graph tour in v in

  let gen (n: int) =
    let graph = Dcgraph.random_dcgraph_3 n 15 in
    let sol = Tdtsp.glouton_2 graph Dcgraph.first in
    graph, sol
  in

  let cmp = Bench.compare 20 gen [gl; evo; sa1] in
  Printf.printf "%s" cmp
  (* let file = open_out "./bench/evo/exec.dat" in *)
  (* for i = 2 to 30 do *)
  (*   let n = 6 * i in *)
  (*   let time = bench_evo n in *)
  (*   Printf.fprintf file "%s" time; *)
  (*   flush file; *)
  (* done *)

