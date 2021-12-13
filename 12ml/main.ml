
module Graph = Map.Make(String);;


let add_cave graph line =
  match (String.split_on_char '-' line) with
  | from_cave :: to_cave :: [] ->
      let graph =
        if from_cave = "start"
          then
            (if (Graph.mem to_cave graph)
              then graph
            else Graph.add to_cave [] graph)
        else if (Graph.mem to_cave graph)
          then (Graph.add to_cave (from_cave :: (Graph.find to_cave graph)) graph)
        else (Graph.add to_cave [from_cave] graph)
      in
        if to_cave = "start"
          then
            (if (Graph.mem from_cave graph)
              then graph
            else Graph.add from_cave [] graph)
        else if (Graph.mem from_cave graph)
          then (Graph.add from_cave (to_cave :: (Graph.find from_cave graph)) graph)
        else
          Graph.add from_cave [to_cave] graph
  | _ -> raise (Failure "unexpected edge line")
;;

let get_caves_graph lines =
  let graph = Graph.empty in
    let rec aux graph lines =
      match lines with
      | [] -> graph
      | head :: rest -> aux (add_cave graph head) rest
    in
      aux graph lines
;;

let is_cave_big cave =
  String.uppercase_ascii cave = cave
;;

let flatten ll =
  let rec go acc = function
  | [] -> List.rev acc
  | l :: r -> go (List.rev_append l acc) r
in
  go [] ll
;;

let path_count_cave path cave =
  List.length (List.filter (fun c -> c = cave) path)
;;

let path_has_small_cave_twice path =
  List.filter
  (fun cave ->
    not (is_cave_big cave) && (path_count_cave path cave = 2))
  path
  != []
;;

let get_cave_paths graph =
  let rec aux cave cur_path =
    match cave with
    | "end" -> [cur_path @ ["end"]]
    | cave ->
        let new_path = cur_path @ [cave] in
          flatten (
            List.map
            (fun to_cave ->
              if (is_cave_big to_cave)
                then aux to_cave new_path
              else (
                (* if List.exists (String.equal to_cave) new_path
                  then [] *)
                if (path_has_small_cave_twice new_path
                    && (List.exists (String.equal to_cave) new_path))
                  then []
                else aux to_cave new_path))
            (Graph.find cave graph)
          )
  in
    aux "start" []
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


let caves_graph = get_caves_graph all_lines;;

let cave_paths = get_cave_paths caves_graph;;

let path_count = List.length cave_paths;;

let () = print_int path_count;;
let () = print_newline ();;


let () = close_in file_channel;;
