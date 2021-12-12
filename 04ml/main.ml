open Str;;

type cell = { marked : bool; value : int };;
type board = { rows : cell list list };;

let get_cell board rn cn =
  List.nth (List.nth board.rows rn) cn
;;

let replace_cell cells rn cn cell =
  List.mapi 
    (fun index row -> 
      if index = rn
        then List.mapi
          (fun index col -> if index = cn then cell else col)
          row
      else row)
    cells
;;

let mark_cell board rn cn = {
  rows = replace_cell
    board.rows rn cn {
      (get_cell board rn cn) with marked=true
    }
};;


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

let line_to_row line =
  let numbers = List.map int_of_string (split (regexp "[ \n\r\x0c\t]+") line) in
    List.map (fun n -> { marked = false; value = n }) numbers
;;

let add_line_to_board board line =
  { rows = board.rows @ [line_to_row line] }
;;

let get_boards_from_lines lines =
  let rec get_boards lines cur_board =
    let cur_board =
      match cur_board with
      | None -> { rows = [] }
      | Some board -> board
    in
      match lines with
      | [] -> [cur_board]
      | head :: rest ->
          if head = "" then cur_board :: get_boards rest None
          else get_boards rest (Some (add_line_to_board cur_board head))
  in
    get_boards lines None
;;

let mark_row_with_number row number =
  List.map
    (fun cell ->
      if cell.value = number then { cell with marked=true }
      else cell)
    row
;;

let mark_board_with_number board number =
  { rows = List.map (fun row -> mark_row_with_number row number) board.rows }
;;

let mark_boards_from_number boards number =
  List.map (fun board -> mark_board_with_number board number) boards
;;

let rec has_all_cells_marked row =
  match row with
  | [] -> true
  | head :: rest ->
      if head.marked then has_all_cells_marked rest else false
;;

let has_winning_row board =
  let winning_rows = List.filter has_all_cells_marked board.rows in
    match winning_rows with
    | [] -> false
    | _ -> true
;;

let rec is_winning_column rows i =
  match rows with
  | [] -> true
  | head :: tail ->
      if (List.nth head i).marked then is_winning_column tail i else false
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let has_winning_column board =
  let winning_columns = List.filter
    (fun i -> is_winning_column board.rows i)
    (range 0 (List.length (List.nth board.rows 0) - 1))
  in
    match winning_columns with
    | [] -> false
    | _ -> true
;;

let is_winning_board board =
  let won = has_winning_row board in
    if won then won else has_winning_column board
;;

let rec get_winning_board boards =
  match boards with
  | [] -> None
  | head :: rest ->
      if is_winning_board head then (Some head) else get_winning_board rest
;;

let rec get_winning_board_from_drawing boards drawing =
  match drawing with
  | [] -> (None, None)
  | head :: rest ->
      let boards = mark_boards_from_number boards head in
        match get_winning_board boards with
        | None -> get_winning_board_from_drawing boards rest
        | Some board -> (Some board, Some head)
;;

let rec get_all_winning_boards_from_drawing boards drawing =
  match drawing with
  | [] -> []
  | head :: rest ->
      let boards = mark_boards_from_number boards head in
        let not_winning = List.filter (fun board -> if is_winning_board board then false else true) boards in
          List.map (fun board -> (board, head)) (List.filter is_winning_board boards)
            @ get_all_winning_boards_from_drawing not_winning rest
;;


let rec flatten list =
  match list with
  | [] -> []
  | head :: rest ->
      head @ flatten rest
;;

let get_board_score board number =
  let factors = List.map
    (fun row -> List.map (fun cell -> if cell.marked then 0 else cell.value) row)
    board.rows
  in
    (List.fold_left (fun a b -> a + b) 0 (flatten factors)) * number
;;


let list_from list start =
  List.filteri (fun i _ -> if i >= start then true else false) list
;;

let file_channel = open_in("ex.txt");;
let all_lines = input_lines file_channel;;

let drawing = List.map int_of_string (String.split_on_char ',' (List.nth all_lines 0));;

let board_lines = list_from all_lines 2;;
let boards = get_boards_from_lines (board_lines);;

let board, number = get_winning_board_from_drawing boards drawing;;

(* let () = print_endline (string_of_int (Option.get number));; *)

let () = print_endline (string_of_int (get_board_score (Option.get board) (Option.get number)));;


let all_winning = get_all_winning_boards_from_drawing boards drawing;;

let last_board, last_number = List.nth all_winning (List.length all_winning - 1);;

let () = print_endline (string_of_int (get_board_score last_board last_number));;


let () = close_in file_channel;;
