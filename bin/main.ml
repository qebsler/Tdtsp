open Utils.Tps
(* open Graph *)
(* NOTE: data pour tester -> http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/ *)

let () =
  Random.self_init ();

  compute_exec_times "bench/naif.dat" "bench/gloutons.dat" "bench/other.dat";
