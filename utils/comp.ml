(* open Bench *)
open Graph.Dcgraph
open Graph.Tdtsp

let compute_little_graph (gen: int -> 'a) (size: int) (nb_test: int) (filename: string): unit =
  let period = 100 in
  let fn_list = [naif_2; glouton_2 (min_graph period); glouton_2 (mean_graph period); glouton_2 (max_graph period)]
    @ List.init 10 (fun _ -> sa_1 (mean_graph period))
    @ List.init 10 (fun _ -> sa_2 (mean_graph period))
    @ List.init 10 (fun _ -> evo 40 (mean_graph period)) in
  let file = open_out filename in

  for _ = 1 to nb_test do
    let graph = gen size in
    List.map (fun fn -> let _, value = fn graph in string_of_int value) fn_list
      |> String.concat "\t"
      |> Printf.fprintf file "%s\n";
    flush file;
  done;
  close_out file

let compute_big_graph (gen: int -> 'a) (size: int) (nb_test: int) (filename: string): unit =
  let period = 100 in
  let fn_list = [glouton_2_parallele (min_graph period); glouton_2_parallele (mean_graph period); glouton_2_parallele (max_graph period)]
    @ List.init 20 (fun _ -> sa_1 (mean_graph period)) @ List.init 20 (fun _ -> sa_1 (min_graph period))
    @ List.init 20 (fun _ -> sa_2 (mean_graph period)) @ List.init 20 (fun _ -> sa_2 (min_graph period))
    @ List.init 20 (fun _ -> evo 40 (mean_graph period)) @ List.init 20 (fun _ -> evo 40 (min_graph period))
  in
  let file = open_out filename in

  for _ = 1 to nb_test do
    let graph = gen size in
    List.map (fun fn -> let _, value = fn graph in string_of_int value) fn_list
      |> String.concat "\t"
      |> Printf.fprintf file "%s\n";
    flush file;
  done;
  close_out file
