  open Bench
  open Graph.Tdtsp
  open Graph.Dcgraph
  open Graph.Gen
  open Graph.Sa

  let compute_exec_times (filename_naif: string) (filename_glouton: string) (filename_other: string): unit =
    let gen = fun n -> random_euclidian_dcgraph n 15 in
    let size_naif = List.init 2 ((+) 4) in  (* TODO: Update *)
    let fn_naif_list = [naif_1; fun graph -> let _, value = naif_2 graph in value] in
    let size_glouton = List.init 4 (fun i -> 5 * (i + 2)) in (* TODO: Update*)
    let fn_glouton_list = [(glouton_1 first); (glouton_2 first); (glouton_2_parallele first)] in
    let prec_gloutons = List.init 3 (fun _ -> 1.) in 
    (* Algos de parcours exhaustifs *)
    let naif_file = open_out filename_naif in 
    Printf.fprintf naif_file "n\tnaif_1(100)\tnaif_2(100)\n";
    List.iter (fun n -> multiple_bench n gen fn_naif_list [100.; 100.] |> Printf.fprintf naif_file "%s\n"; flush naif_file) size_naif;
    close_out naif_file;

    (* Algos gloutons *)
    let glouton_file = open_out filename_glouton in 
    Printf.fprintf glouton_file "n\tglouton_1\tglouton_2\tglouton_2_parallele\n";
    List.iter (fun n -> multiple_bench n gen fn_glouton_list prec_gloutons |> Printf.fprintf glouton_file "%s\n"; flush glouton_file) size_glouton;
    close_out glouton_file;
    
    (* Autres algos *)
    let sa = fun graph -> let tour, _ = glouton_2 first graph in sa_default_settings graph tour in 
    let evo = fun graph -> let sol = glouton_2 first graph in evolutionnary graph 40 sol in 
    let fn_list = [sa; evo] in 
    let size = List.init 2 (fun i -> 5 * (i + 2)) in  (* TODO: Update *)
    let prec = [1.; 1.] in 
    let file = open_out filename_other in 
    Printf.fprintf file "n\tsim_annealing\tevo(40gen)\n";
    List.iter (fun n -> multiple_bench n gen fn_list prec |> Printf.fprintf file "%s\n"; flush file) size;
    close_out file;
