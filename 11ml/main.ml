


(* this is the most beautiful/awful FP function I've ever created *)
let create_matrix lines =
  let matrix = Array.make_matrix (List.length lines) (List.length lines) 0 in
    for row_n = 0 to (List.length lines - 1) do
      let char_list = List.nth lines row_n |> String.to_seq |> List.of_seq in
        for col_n = 0 to (List.length char_list - 1) do
          matrix.(row_n).(col_n) <- (List.nth char_list col_n) |> String.make 1 |> int_of_string
        done
    done;
    matrix
;;

let rec emit_flash_perhaps matrix row_n col_n flash_count =
  if matrix.(row_n).(col_n) > 9
    then
      (incr flash_count;
      matrix.(row_n).(col_n) <- 0;
      apply_adjacent_flash matrix row_n col_n flash_count)
  else ()

and apply_adjacent_flash matrix row_n col_n flash_count =
  (try (increase_not_zero matrix (row_n-1) col_n flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix (row_n+1) col_n flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix row_n (col_n-1) flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix (row_n-1) (col_n-1) flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix (row_n+1) (col_n-1) flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix row_n (col_n+1) flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix (row_n-1) (col_n+1) flash_count) with
  | Invalid_argument (_) -> ());
  (try (increase_not_zero matrix (row_n+1) (col_n+1) flash_count) with
  | Invalid_argument (_) -> ())

and increase_not_zero matrix row_n col_n flash_count =
  if matrix.(row_n).(col_n) <> 0
    then (
      matrix.(row_n).(col_n) <- matrix.(row_n).(col_n) + 1;
      emit_flash_perhaps matrix row_n col_n flash_count)
  else ()
;;

let apply_step matrix =
  for row_n = 0 to (Array.length matrix - 1) do
    for col_n = 0 to (Array.length matrix.(row_n) - 1) do
      matrix.(row_n).(col_n) <- matrix.(row_n).(col_n) + 1
    done
  done;
  let flash_count = ref 0 in
    for row_n = 0 to (Array.length matrix - 1) do
      for col_n = 0 to (Array.length matrix.(row_n) - 1) do
        emit_flash_perhaps matrix row_n col_n flash_count
      done
    done;
    !flash_count
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b
;;

let apply_n_steps matrix n =
  List.fold_left ( + ) 0 (List.map (fun _ -> apply_step matrix) (range 0 (n-1)))
;;

let synchronicity matrix =
  Array.for_all
  (fun row ->
    Array.for_all
    (fun energy -> energy = 0)
    row)
  matrix
;;

let apply_steps_until_synchronicity matrix =
  let count = ref 0 in
    let quit_loop = ref false in
      while not !quit_loop do
        incr count;
        let _ = apply_step matrix in
          if synchronicity matrix then
            quit_loop := true
          else ()
      done;
      !count
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

let matrix = create_matrix all_lines;;

(* let flash_count = apply_n_steps matrix 100;;

(* let matrix = matrix;; *)

let () = print_int flash_count;;
let () = print_newline ();; *)

let step_count = apply_steps_until_synchronicity matrix;;

let () = print_int step_count;;
let () = print_newline ();;


let () = close_in file_channel;;
