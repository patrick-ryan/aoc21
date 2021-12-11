let sort s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq;;

let flatten ll =
  let rec go acc = function
  | [] -> List.rev acc
  | l :: r -> go (List.rev_append l acc) r
in
  go [] ll
;;

let get_output_unique_signal_count outputs unique_signal_lengths =
  List.fold_left
  (fun acc output ->
    match (List.find_opt (fun x -> x = String.length output) unique_signal_lengths) with
    | None -> acc+0
    | Some _ -> acc+1)
  0
  (flatten outputs)
;;

let get_one digits =
  List.find (fun s -> String.length s = 2) digits
;;

let get_seven digits =
  List.find (fun s -> String.length s = 3) digits
;;

let contains_one_of s1 s2 =
  if String.contains s1 (String.get s2 0)
    then not (String.contains s1 (String.get s2 1))
  else
    not (String.contains s1 (String.get s2 0))
;;

let get_six digits one =
  List.find (fun s -> String.length s = 6 && (contains_one_of s one)) digits
;;

let get_four digits =
  List.find (fun s -> String.length s = 4) digits
;;

let get_eight digits =
  List.find (fun s -> String.length s = 7) digits
;;

let get_zero digits four_part =
  List.find (fun s -> String.length s = 6 && (contains_one_of s four_part)) digits
;;

let get_nine digits six zero =
  List.find (fun s -> String.length s = 6 && (sort s <> sort six) && (sort s <> sort zero)) digits
;;

let str_contains s chars =
  List.for_all (fun chr -> String.exists (fun c -> chr = c) s) chars
;;

let get_two digits parts =
  List.find (fun s -> String.length s = 5 && (str_contains s parts)) digits
;;

let get_three digits parts  =
  List.find (fun s -> String.length s = 5 && (str_contains s parts)) digits
;;

let get_five digits parts  =
  List.find (fun s -> String.length s = 5 && (str_contains s parts)) digits
;;


let get_remaining s1 s2 =
  let s1_list = String.to_seq s1 |> List.of_seq in
    List.find (fun chr -> String.contains s2 chr = false) s1_list
;;

let without list chr =
  List.filter (fun c -> c <> chr) list
;;

let get_remaining_str s1 s2 =
  let rec aux s1_list s2 chars =
    match s1_list with
    | [] -> chars
    | rest ->
        let remaining = List.find_opt (fun chr -> String.contains s2 chr = false) rest in
          match remaining with
          | None -> chars
          | Some chr -> aux (without rest chr) s2 (chr :: chars)
  in
    aux (String.to_seq s1 |> List.of_seq) s2 []
      |> List.to_seq |> String.of_seq
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b
;;

let get_signal_map inputs outputs =
  let digits = (inputs @ outputs)
  and signal_map = Hashtbl.create 7 in
    let one = get_one digits
    and seven = get_seven digits
    and eight = get_eight digits
    and four = get_four digits in
      let top_char = get_remaining seven one in
        let six = get_six digits one in
          let top_right_char = get_remaining one six in
            let bottom_right_char = get_remaining one (String.make 1 top_right_char) in
              let zero = get_zero digits (get_remaining_str four one) in
                let nine = get_nine digits six zero in
                  let middle_char = get_remaining eight zero in
                    let bottom_left_char = get_remaining eight nine in
                      let two = get_two digits [top_right_char; bottom_left_char; middle_char; top_char] in
                        let bottom_char = get_remaining two ([top_right_char; bottom_left_char; middle_char; top_char] |> List.to_seq |> String.of_seq) in
                          let three = get_three digits [top_right_char; bottom_right_char; bottom_char; middle_char; top_char] in
                            let top_left_char = get_remaining four ([top_right_char; bottom_right_char; middle_char;] |> List.to_seq |> String.of_seq) in
                              let five = [top_left_char; bottom_right_char; bottom_char; middle_char; top_char] |> List.to_seq |> String.of_seq in
                                Hashtbl.add signal_map (sort zero) '0';
                                Hashtbl.add signal_map (sort one) '1';
                                Hashtbl.add signal_map (sort two) '2';
                                Hashtbl.add signal_map (sort three) '3';
                                Hashtbl.add signal_map (sort four) '4';
                                Hashtbl.add signal_map (sort five) '5';
                                Hashtbl.add signal_map (sort six) '6';
                                Hashtbl.add signal_map (sort seven) '7';
                                Hashtbl.add signal_map (sort eight) '8';
                                Hashtbl.add signal_map (sort nine) '9';
                                signal_map
;;

let decode_output signal_map output =
  Hashtbl.find signal_map (sort output)
;;

let decode_outputs signal_map outputs =
  int_of_string (List.map (fun o -> decode_output signal_map o) outputs |> List.to_seq |> String.of_seq)
;;

let get_numbers inputs outputs =
  List.mapi
  (fun i input_group ->
    let signal_map = get_signal_map input_group (List.nth outputs i) in
      decode_outputs signal_map (List.nth outputs i))
  inputs
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

let inputs = List.map (fun line -> String.split_on_char ' ' (List.nth (Str.split (Str.regexp " | ") line) 0)) all_lines;;
let outputs = List.map (fun line -> String.split_on_char ' ' (List.nth (Str.split (Str.regexp " | ") line) 1)) all_lines;;

let unique_signal_lengths = [7; 3; 2; 4;];;

let count = get_output_unique_signal_count outputs unique_signal_lengths;;

(* let () = print_endline (string_of_int count);; *)

let numbers = get_numbers inputs outputs;;

let () = print_endline (string_of_int (List.fold_left ( + ) 0 numbers));;


let () = close_in file_channel;;

