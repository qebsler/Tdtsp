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
    ("n\tnaif_1(100)\tnaif_2(100)") :: List.map (fun n -> multiple_bench n gen fn_naif_list [1000.; 1000.]) size_naif
    |> String.concat "\n"
    |> Printf.fprintf naif_file "%s";
    close_out naif_file;

    (* Algos gloutons *)
    let glouton_file = open_out filename_glouton in 
    ("n\tglouton_1\tglouton_2\tglouton_2_parallele") :: List.map (fun n -> multiple_bench n gen fn_glouton_list prec_gloutons) size_glouton
    |> String.concat "\n"
    |> Printf.fprintf glouton_file "%s";
    close_out glouton_file;
    
    (* Autres algos *)
    let sa = fun graph -> let tour, _ = glouton_2 first graph in sa_default_settings graph tour in 
    let evo = fun graph -> let sol = glouton_2 first graph in evolutionnary graph 40 sol in 
    let fn_list = [sa; evo] in 
    let size = List.init 2 (fun i -> 5 * (i + 2)) in  (* TODO: Update *)
    let prec = [1.; 1.] in 
    let file = open_out filename_other in 
    ("n\tsim_annealing\tevo (40gen)") :: List.map (fun n -> multiple_bench n gen fn_list prec) size 
    |> String.concat "\n"
    |> Printf.fprintf file "%s";
    close_out file;
