
exception CorruptChunk of char;;

(* type chunk =
  | ParenChunk of (chunk list)
  | BracketChunk of (chunk list)
  | CurlyChunk of (chunk list)
  | AnglyChunk of (chunk list)
;;

let build_chunks chars =
  let rec aux chars prev_first_char_opt first_char_opt cur_chunk_parts =
    match chars with
    | [] -> (
        match prev_first_char_opt with
        | None -> cur_chunk_parts
        | Some _ -> [])  (* incomplete *)
    | head :: rest ->
        match first_char_opt with
        | None -> (
            match head with
            | '(' -> aux rest None (Some head) []
            | _ -> raise (Failure "1"))
        | Some '(' -> (
            match head with
            | ')' -> aux rest None prev_first_char_opt [ParenChunk cur_chunk_parts]
            | '(' -> aux rest first_char_opt (Some head) []
            | '}' -> raise (CorruptChunk head)  (* corrupt *)
            | _ -> raise (Failure "2"))
        | Some _ -> raise (Failure "3")
  in
    aux chars None None []
;; *)

let index_exists l i =
  if i = (-1) then false
  else
    match (List.nth_opt l i) with
    | None -> false
    | Some _ -> true
;;

let build_chunk_stack chars =
  let rec aux chars stack =
    match chars with
    | [] -> stack
    | head :: rest ->
        (match head with
        | ('('|'{'|'['|'<') -> aux rest (head :: stack)
        | ')' ->
            (match stack with
            | [] -> raise (CorruptChunk head)
            | stack_head :: stack_rest ->
                (match stack_head with
                | '(' -> aux rest stack_rest
                | _ -> raise (CorruptChunk head)))  (* corrupt *)
        | '}' ->
            (match stack with
            | [] -> raise (CorruptChunk head)
            | stack_head :: stack_rest ->
                (match stack_head with
                | '{' -> aux rest stack_rest
                | _ -> raise (CorruptChunk head)))  (* corrupt *)
        | ']' ->
            (match stack with
            | [] -> raise (CorruptChunk head)
            | stack_head :: stack_rest ->
                (match stack_head with
                | '[' -> aux rest stack_rest
                | _ -> raise (CorruptChunk head)))  (* corrupt *)
        | '>' ->
            (match stack with
            | [] -> raise (CorruptChunk head)
            | stack_head :: stack_rest ->
                (match stack_head with
                | '<' -> aux rest stack_rest
                | _ -> raise (CorruptChunk head)))  (* corrupt *)
        | _ -> raise (CorruptChunk head) )
  in
    aux chars []
;;


let get_char_points chr =
  match chr with
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> raise (Failure "where did this come from??")
;;


let get_line_corrupted_error_score line =
  let chars = line |> String.to_seq |> List.of_seq in
    let score, _ = 
      try (Some 0, Some (build_chunk_stack chars)) with
        CorruptChunk (chr) -> (Some (get_char_points chr), None)
    in
      Option.get score
;;

let get_completion_char_points chr =
  match chr with
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> raise (Failure "where did this come from??")
;;

let get_complement_char chr =
  match chr with
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | _ -> raise (Failure "where did this come from??")
;;

let get_stack_incomplete_error_score stack =
  List.fold_left
  (fun acc chr -> (acc*5) + get_completion_char_points (get_complement_char chr))
  0
  stack
;;


let get_line_incomplete_error_score line =
let chars = line |> String.to_seq |> List.of_seq in
  let stack = 
    try Some (build_chunk_stack chars) with
      CorruptChunk (_) -> None
  in
    match stack with
    | None -> None
    | Some stack -> Some (get_stack_incomplete_error_score stack)
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

let line_scores = List.map get_line_corrupted_error_score all_lines;;

let total_score = List.fold_left ( + ) 0 line_scores;;

let () = print_int total_score;;
let () = print_newline ();;

let line_incomplete_scores = List.filter_map (fun line -> get_line_incomplete_error_score line) all_lines;; 
let middle_score =
  List.nth
  (List.sort Int.compare line_incomplete_scores)
  (List.length line_incomplete_scores / 2);;

let () = print_int middle_score;;
let () = print_newline ();;



let () = close_in file_channel;;
