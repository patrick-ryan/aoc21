
let flatten ll =
  let rec go acc = function
  | [] -> List.rev acc
  | l :: r -> go (List.rev_append l acc) r
in
  go [] ll
;;

let get_matrix lines =
  List.map
  (fun l ->
    List.map
    (fun c -> int_of_string (String.make 1 c))
    (l |> String.to_seq |> List.of_seq))
  lines
;;

let index_exists l i =
  if i = (-1) then false
  else
    match (List.nth_opt l i) with
    | None -> false
    | Some _ -> true
;;

let coord_exists matrix row_num col_num =
  if index_exists matrix row_num
    then index_exists (List.nth matrix row_num) col_num
  else false
;;

let get_coord matrix row_num col_num =
  List.nth (List.nth matrix row_num) col_num
;;

let get_adjacents matrix row_num col_num =
  []
    @
    (if coord_exists matrix (row_num-1) (col_num) then [get_coord matrix (row_num-1) (col_num)]
    else [])
    @
    (if coord_exists matrix (row_num) (col_num-1) then [get_coord matrix (row_num) (col_num-1)]
    else [])
    @
    (if coord_exists matrix (row_num) (col_num+1) then [get_coord matrix (row_num) (col_num+1)]
    else [])
    @
    (if coord_exists matrix (row_num+1) (col_num) then [get_coord matrix (row_num+1) (col_num)]
    else [])
;;

let get_low_points matrix =
  flatten (
    List.mapi
    (fun row_num row ->
      List.filteri
      (fun col_num point ->
        let adjacents = get_adjacents matrix row_num col_num in
          if List.for_all (fun a -> a > point) adjacents then true
          else false)
      row)
    matrix
  )
;;

let rec get_basin_coords_for_point matrix row_num col_num =
  if get_coord matrix row_num col_num = 9 then []
  else
    let point = get_coord matrix row_num col_num in
      [[row_num;col_num]]
      @ (if (coord_exists matrix (row_num-1) col_num) && ((get_coord matrix (row_num-1) col_num) > point)
          then get_basin_coords_for_point matrix (row_num-1) col_num
        else [])
      @ (if (coord_exists matrix row_num (col_num-1)) && ((get_coord matrix row_num (col_num-1)) > point)
          then get_basin_coords_for_point matrix row_num (col_num-1)
        else [])
      @ (if (coord_exists matrix (row_num+1) col_num) && ((get_coord matrix (row_num+1) col_num) > point)
          then get_basin_coords_for_point matrix (row_num+1) col_num
        else [])
      @ (if (coord_exists matrix row_num (col_num+1)) && ((get_coord matrix row_num (col_num+1)) > point)
          then get_basin_coords_for_point matrix row_num (col_num+1)
        else [])
;;

let rec uniq x =
  let rec uniq_help l n = 
    match l with
    | [] -> []
    | h :: t -> if (List.equal Int.equal n h) then uniq_help t n else h::(uniq_help t n) in
  match x with
  | [] -> []
  | h::t -> h::(uniq_help (uniq t) h)
;;

let get_basin_sizes matrix =
  flatten (
    List.mapi
    (fun row_num row ->
      List.mapi
      (fun col_num point ->
        let adjacents = get_adjacents matrix row_num col_num in
          if List.for_all (fun a -> a > point) adjacents then
            List.length (uniq (get_basin_coords_for_point matrix row_num col_num))
          else 1)
      row)
    matrix
  )
;;

let get_basin_factor matrix =
  List.fold_left ( * ) 1
  (List.filteri (fun i _ -> i < 3) (List.rev (List.sort Int.compare (get_basin_sizes matrix))))
;;



let maybe_input_line stdin =
  try Some (input_line stdin) with
    End_of_file -> None
;;

let input_lines stdin =
  let rec input lines =
    match maybe_input_line stdin with
      Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []
;;

let file_channel = open_in("in.txt");;
let all_lines = input_lines file_channel;;

let matrix = get_matrix all_lines;;

let low_points = get_low_points matrix;;
let total_risk = List.fold_left (fun acc point -> acc + point + 1) 0 low_points;;

let () = print_endline (string_of_int total_risk);;

let basin_factor = get_basin_factor matrix;;

let () = print_endline (string_of_int basin_factor);;


let () = close_in file_channel;;
