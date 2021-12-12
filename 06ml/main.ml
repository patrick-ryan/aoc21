
type fish = { cycle_days_left : int };;


let rec range a b =
  if a > b then []
  else a :: range (a+1) b
;;

let map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] l
;;

let flatten2 ll =
  let rec go acc = function
  | [] -> List.rev acc
  | l :: r -> go (List.rev_append l acc) r
in
  go [] ll
;;

let _clone fishes =
  let fish_groups = map
    (fun fish ->
      if fish.cycle_days_left = 0
        then [{ cycle_days_left = 6 }; { cycle_days_left = 8 }]
      else [{ cycle_days_left = fish.cycle_days_left - 1 }])
    fishes
  in
    flatten2 fish_groups
;;


let clone_fish fishes =
  _clone fishes

;;

let rec cycle_fish fishes days =
  if days = 0 then fishes else cycle_fish (clone_fish fishes) (days-1)
;;

let init_hist number =
  Array.of_list (List.map (fun _ -> 0) (range 0 number))
;;

let add_hist_number hist number =
  hist.(number) <- hist.(number) + 1
;;

let adjust_hist_day hist =
  let zeros_count = hist.(0) in
    List.map
    (fun number ->
      if number = 0
        then
          hist.(number) <- 0
      else if number = 6
        then
          let _ = hist.(number-1) <- hist.(number-1) + hist.(number)
          and _ = hist.(number) <- zeros_count in ()
      else if number = (Array.length hist - 1)
        then
          let _ = hist.(number-1) <- hist.(number-1) + hist.(number)
          and _ = hist.(number) <- zeros_count in ()
      else
        let _ = hist.(number-1) <- hist.(number-1) + hist.(number)
        and _ = hist.(number) <- 0 in () )
    (range 0 (Array.length hist - 1))
;;

let rec cycle_fish_hist hist days =
  if days = 0 then hist else let _ = adjust_hist_day hist in cycle_fish_hist hist (days-1)
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

let init_fish_ints = List.map int_of_string (String.split_on_char ',' (List.nth all_lines 0));;
let init_fish = List.map (fun i -> { cycle_days_left = i }) init_fish_ints;;

let hist = init_hist 8;;
let _ = List.map (fun number -> add_hist_number hist number) init_fish_ints;;

let days = 256;;
(* let final_fish = cycle_fish init_fish days;; *)

(* let () = print_endline (string_of_int (List.length final_fish));; *)

let _ = cycle_fish_hist hist days;;
let final_fish_count = Array.fold_left ( + ) 0 hist;;

let () = print_endline (string_of_int final_fish_count);;


let () = close_in file_channel;;
