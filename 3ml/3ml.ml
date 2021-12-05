(* open Printf *)

let maybe_input_line stdin =
  try Some (input_line stdin) with
    End_of_file -> None;;

let input_lines stdin =
  let rec input lines =
    match maybe_input_line stdin with
      Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input [];;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let col l i =
  List.map (fun elem -> int_of_string (String.make 1 elem.[i])) l;;

let cols lines =
  match lines with
    [] -> []
  | rest ->
      List.map (fun i -> col rest i) (range 0 (String.length (List.nth rest 0)-1));;

let count_occurrences num l =
  List.fold_left (fun a b -> if b = num then a+1 else a) 0 l;;

let num_list list =
  List.map int_of_string (List.map (String.make 1) list);;

let uniq num_list =
  List.sort_uniq Int.compare num_list;;

let get_counts num_list =
  List.map (fun a -> [a;count_occurrences a num_list]) (uniq num_list);;

let most_sig_number col_counts =
  List.fold_left (fun highest b -> if ((List.nth highest 1 = List.nth b 1) && (List.nth b 0 = 1)) then b else if List.nth b 1 > List.nth highest 1 then b else highest) [0;0] col_counts;;

let least_sig_number col_counts max_count =
  List.fold_left (fun lowest b -> if ((List.nth lowest 1 = List.nth b 1) && (List.nth b 0 = 0)) then b else if List.nth lowest 1 > List.nth b 1 then b else lowest) [0;max_count+1] col_counts;;

let most_sigs cols =
  List.map most_sig_number (List.map get_counts cols);;

let least_sigs cols max_count =
  List.map (fun col_count -> least_sig_number col_count max_count) (List.map get_counts cols);;

let counts_to_bin counts =
  "0b" ^ String.concat "" (List.map (fun b -> string_of_int (List.nth b 0)) counts);;

let bin_to_decimal bin =
  int_of_string bin;;

let filter_by_first_bit rows bit =
  if (String.length (List.nth rows 0)) = 1 then [] else
  List.map (fun row -> String.sub row 1 ((String.length row)-1)) (List.filter (fun row -> row.[0] = bit) rows);;

let rec get_most_sig_bin rows =
  let cols = cols rows in
  match cols with
    [] -> ""
  | col :: _ ->
      let most_sig_bit = List.nth (most_sig_number (get_counts col)) 0 in
        (string_of_int most_sig_bit) ^ get_most_sig_bin (filter_by_first_bit rows (string_of_int most_sig_bit).[0])
;;

let rec get_least_sig_bin rows max_count =
  let cols = cols rows in
  match cols with
    [] -> ""
  | col :: _ ->
      let least_sig_bit = List.nth (least_sig_number (get_counts col) max_count) 0 in
        (string_of_int least_sig_bit) ^ get_least_sig_bin (filter_by_first_bit rows (string_of_int least_sig_bit).[0]) max_count
;;


(* let file_channel = open_in("3.ex.txt");; *)
let file_channel = open_in("3.in.txt");;
let all_lines = input_lines file_channel;;
(* let () = List.iter (printf "%s\n") all_lines;; *)

let columns = cols all_lines;;
let gamma_rate = bin_to_decimal (counts_to_bin (most_sigs columns));;
let max_count = List.length (List.nth columns 0);;
let epsilon_rate = bin_to_decimal (counts_to_bin (least_sigs columns max_count));;
let power_consumption = gamma_rate * epsilon_rate;;

let () = print_endline (string_of_int power_consumption);;



let oxygen_generator_rating = bin_to_decimal ("0b" ^ get_most_sig_bin all_lines);;
let co2_scrubber_rating = bin_to_decimal ("0b" ^ get_least_sig_bin all_lines max_count);;
let life_support_rating = oxygen_generator_rating * co2_scrubber_rating;;

let () = print_endline (string_of_int life_support_rating);;



let () = close_in file_channel;;