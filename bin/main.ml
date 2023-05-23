open Utils.Comp
open Graph.Gen
(* open Graph *)
(* NOTE: data pour tester -> http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/ *)

let () =
  Random.self_init ();

  let sizes = [|50; 100; 200; 300; 400|] in
  let nb_test = 1 in (* TODO: Update *)

  compute_little_graph (random_dcgraph 100) 10 nb_test "bench/comp/comp10.dat";

  for i = 0 to Array.length sizes - 1 do
    let size = sizes.(i) in
    Printf.sprintf "bench/comp/comp%d_random.dat" size
      |> compute_big_graph (random_dcgraph 100) size nb_test;

    Printf.sprintf "bench/comp/comp%d_static.dat" size
      |> compute_big_graph random_static_dcgraph size nb_test;

    Printf.sprintf "bench/comp/comp%d_euclidian.dat" size
      |> compute_big_graph random_euclidian_dcgraph_varia size nb_test;

    Printf.sprintf "bench/comp/comp%d_echelon.dat" size
      |> compute_big_graph random_dcgraph_echelon_region size nb_test;

    Printf.sprintf "bench/comp/comp%d_euclidian_echelon.dat" size
      |> compute_big_graph random_euclidian_dcgraph_echelon_region size nb_test;
  done

