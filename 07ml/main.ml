

let cost_f count =
  let rec aux count prev_cost step_cost =
    if count = 0 then prev_cost
    else aux (count-1) (prev_cost+step_cost) (step_cost+1)
  in
    aux count 0 1
;;

let distances_from_number numbers number =
  List.map (fun x -> cost_f (Int.abs (number - x))) numbers
;;

let total_cost_from_number numbers number =
  List.fold_left ( + ) 0 (distances_from_number numbers number)
;;

let halfway numbers =
  (List.length numbers - 1)/2
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b
;;

let last list =
  List.nth list (List.length list - 1)
;;

(* let find_optimal_position positions =
  let rec aux positions spots (best,cost) =
    match spots with
    | [] -> (best, cost)
    | rest ->
        let halfway_n = halfway spots in
          let halfway_cost = total_cost_from_number positions halfway_n in
            if halfway_cost < cost

    (* let halfway_n = List.nth positions (halfway positions) in
      let halfway_cost = total_cost_from_number positions halfway_n
      and halfway_minus1_cost = total_cost_from_number positions halfway_n
        if halfway_cost < halfway_minus1_cost
          then aux positions *)
        
  in
    aux positions (range 0 (last positions)) (0,0)
;; *)


let find_optimal_position positions =
  List.fold_left
  (fun (best,cost) i ->
    let new_cost = total_cost_from_number positions i in
      if cost = -1 || new_cost < cost then (i,new_cost) else (best,cost))
  (-1,-1)
  (range (List.hd positions) (last positions))
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


let positions = List.map int_of_string (String.split_on_char ',' (List.nth all_lines 0));;

let positions = List.sort Int.compare positions ;;

let costs = List.map (fun i -> total_cost_from_number positions i) (range (List.hd positions) (last positions));;

let (optimal_position, cost) = find_optimal_position positions;;

let () = print_endline (string_of_int cost);;
let () = print_endline (string_of_int optimal_position);;


let () = close_in file_channel;;



