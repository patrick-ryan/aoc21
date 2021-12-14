

let get_dimensions_from_coords coords =
  let rec aux coords max_x max_y =
    match coords with
    | [] -> max_x, max_y
    | (x :: y :: []) :: rest -> aux rest (max max_x x) (max max_y y)
    | _ -> raise (Failure "invalid coords")
  in
    aux coords 0 0
;;

let create_matrix_from_coords coords =
  let x,y = get_dimensions_from_coords coords in
    let matrix = Array.make_matrix (x+1) (y+1) false in
      for i = 0 to (List.length coords - 1) do
        matrix.(List.nth (List.nth coords i) 0).(List.nth (List.nth coords i) 1) <- true
      done;
      matrix
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

let fold_up coords fold_y =
  uniq (
    List.map
    (fun coord ->
      match coord with
      | x :: y :: [] -> [x;(if y < fold_y then y else fold_y - (y - fold_y))]
      | _ -> raise (Failure "unexpected coord"))
    coords
  )
;;

let fold_left coords fold_x =
  uniq (
    List.map
    (fun coord ->
      match coord with
      | x :: y :: [] -> [(if x < fold_x then x else fold_x - (x - fold_x));y]
      | _ -> raise (Failure "unexpected coord"))
    coords
  )
;;

let rec apply_folds coords folds =
  match folds with
  | [] -> coords
  | (dir :: position :: []) :: rest ->
      let new_coords =
        if dir = "y" then (fold_up coords (int_of_string position))
        else (fold_left coords (int_of_string position))
      in
        apply_folds new_coords rest
  | _ -> raise (Failure "unexpected fold")
        
;;

let plot_coords coords =
  let matrix = create_matrix_from_coords coords in
    for x = (Array.length matrix - 1) downto 0 do
      for y = 0 to (Array.length matrix.(x) - 1) do
        if matrix.(x).(y) = true
          then print_char '#'
        else print_char '.'
      done;
      print_newline ()
    done
;;



let parse_coord_line line =
  List.map int_of_string (String.split_on_char ',' line)
;;

let parse_fold_line line =
  String.split_on_char '=' (Str.global_replace (Str.regexp "fold along ") "" line)
;;


let maybe_input_line stdin =
  try Some (input_line stdin) with
    End_of_file -> None
;;

let input_lines stdin =
  let rec input coord_lines fold_lines do_folds =
    match maybe_input_line stdin with
    | Some line ->
        if line = ""
          then input coord_lines fold_lines true
        else if not (do_folds)
          then input (line :: coord_lines) fold_lines do_folds
        else input coord_lines (line :: fold_lines) do_folds
    | None -> (List.rev coord_lines, List.rev fold_lines)
  in
  input [] [] false
;;

let file_channel = open_in("in.txt");;
let coord_lines, fold_lines = input_lines file_channel;;

let coords = List.map parse_coord_line coord_lines;;
let folds = List.map parse_fold_line fold_lines;;

(* let new_coords = apply_folds coords [List.nth folds 0];;

let () = print_int (List.length new_coords);;
let () = print_newline ();; *)

let new_coords = apply_folds coords folds;;

let () = plot_coords new_coords;;



let () = close_in file_channel;;
