open Str;;

type point = { number : int };;
type grid = { rows : point list list };;
type segment = { from_x : int; from_y : int; to_x : int; to_y : int };;


let get_grid_dimensions segments =
  let rec get_grid_dimensions segments max_x max_y =
    match segments with
    | [] -> (max_x, max_y)
    | head :: rest ->
        let new_max_x = max head.to_x (max head.from_x max_x)
        and new_max_y = max head.to_y (max head.from_y max_y) in
          get_grid_dimensions rest new_max_x new_max_y
  in
    get_grid_dimensions segments 0 0
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let init_grid (x,y) =
  { rows = List.map
    (fun _ -> List.map (fun _ -> { number = 0 }) (range 0 x))
    (range 0 y)
  }
;;

let is_vertical segment =
  segment.from_x = segment.to_x
;;

let get_vertical_range segment =
  if segment.to_y > segment.from_y then
    (segment.from_y, segment.to_y)
  else
    (segment.to_y, segment.from_y)
;;

let is_horizontal segment =
  segment.from_y = segment.to_y
;;

let get_horizontal_range segment =
  if segment.to_x > segment.from_x then
    (segment.from_x, segment.to_x)
  else
    (segment.to_x, segment.from_x)
;;

let inc_point point =
  { number = point.number + 1 }
;;

let add_grid_vertical grid x_index (y_start, y_end) =
  { rows = List.mapi
      (fun y row ->
        if (y >= y_start && y <= y_end)
          then (List.mapi
            (fun x point -> if x = x_index then inc_point point else point)
            row)
        else row)
      grid.rows
  }
;;

let add_grid_horizontal grid y_index (x_start, x_end) =
  { rows = List.mapi
      (fun y row ->
        if (y = y_index)
          then (List.mapi
            (fun x point -> if (x >= x_start && x <= x_end) then inc_point point else point)
            row)
        else row)
      grid.rows
  }
;;

let get_x_adjusted segment x_start x_end y y_start =
  if ((segment.from_x < segment.to_x) && (segment.from_y < segment.to_y)
      || (segment.from_x > segment.to_x) && (segment.from_y > segment.to_y))
    then (x_start + (y - y_start))
  else
    (x_end - (y - y_start))
;;

let add_grid_diagonal grid segment =
  let (y_start, y_end) = (get_vertical_range segment)
  and (x_start, x_end) = (get_horizontal_range segment) in
    { rows = List.mapi
        (fun y row ->
          if (y >= y_start && y <= y_end)
            then (List.mapi
              (fun x point -> if (x = get_x_adjusted segment x_start x_end y y_start) then inc_point point else point)
              row)
          else row)
        grid.rows
    }
;;

let add_grid_segment grid segment =
  if is_vertical segment then
    add_grid_vertical grid segment.to_x (get_vertical_range segment)
  else if is_horizontal segment then
    add_grid_horizontal grid segment.to_y (get_horizontal_range segment)
  else
    add_grid_diagonal grid segment
;;

let rec add_grid_segments grid segments =
  match segments with
  | [] -> grid
  | head :: rest ->
      add_grid_segments (add_grid_segment grid head) rest
;;



let parse_segment line =
  let parts = split (Str.regexp " -> ") line in
    let from_part = split (Str.regexp ",") (List.nth parts 0)
    and to_part = split (Str.regexp ",") (List.nth parts 1) in
        {
          from_x = int_of_string (List.nth from_part 0);
          from_y = int_of_string (List.nth from_part 1);
          to_x = int_of_string (List.nth to_part 0);
          to_y = int_of_string (List.nth to_part 1)
        }
;;

let rec flatten list =
  match list with
  | [] -> []
  | head :: rest ->
      head @ flatten rest
;;

let get_overlapping_points grid =
  List.filter (fun point -> point.number > 1) (flatten grid.rows)
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

let segments = List.map parse_segment all_lines;;

let grid = init_grid (get_grid_dimensions segments);;
let grid = add_grid_segments grid segments;;

let overapping_points = get_overlapping_points grid;;

let () = print_endline (string_of_int (List.length overapping_points));;




let () = close_in file_channel;;
