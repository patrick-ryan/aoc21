

module InsertionRuleSet = Map.Make(String);;
module InsertionCountSet = Map.Make(Char);;



(* let apply_polymer_step ruleset polymer_chars =
  let rec aux ruleset polymer_chars new_polymer_chars =
    match polymer_chars with
    | char1 :: char2 :: [] ->
        let insertion_element = InsertionRuleSet.find ([char1;char2] |> List.to_seq |> String.of_seq) ruleset in
          char2 :: insertion_element :: char1 :: new_polymer_chars
    | char1 :: char2 :: rest ->
        let insertion_element = InsertionRuleSet.find ([char1;char2] |> List.to_seq |> String.of_seq) ruleset in
          aux ruleset (char2 :: rest) (insertion_element :: char1 :: new_polymer_chars)
    | _ -> raise (Failure "unexpected error applying steps")
  in
    List.rev (aux ruleset polymer_chars [])
;;

let apply_n_polymer_steps ruleset template n =
  let rec aux ruleset n polymer_chars =
    match n with
    | 0 -> polymer_chars
    | _ -> aux ruleset (n-1) (apply_polymer_step ruleset polymer_chars)
  in
    (aux ruleset n (template |> String.to_seq |> List.of_seq))
;;

let get_char_min_max chars =
  let count_char chars c =
    List.fold_left (fun acc chr -> if chr = c then acc+1 else acc) 0 chars
  in
    let rec aux chars found min_max =
      match chars with
      | [] -> min_max
      | chr :: rest ->
          match (List.find_opt (fun c -> c = chr) found) with
          | None ->
              let (min_n, max_n) = min_max in
                let chr_count = count_char chars chr in
                  aux rest (chr :: found) ((min min_n chr_count),(max max_n chr_count))
          | Some _ -> aux rest found min_max
    in
      aux chars [] (List.length chars,1)
;; *)

let get_string_char_n s n =
  List.nth (s |> String.to_seq |> List.of_seq) n
;;

let pair_combos s c =
  (String.make 1 (get_string_char_n s 0)) ^ (String.make 1 c),
  (String.make 1 c) ^ (String.make 1 (get_string_char_n s 1))
;;

let incr_ruleset_pair ruleset pair increment =
  let insertion_element, count = InsertionRuleSet.find pair ruleset in
    InsertionRuleSet.add pair (insertion_element,(count + increment)) ruleset
;;

let incr_countset countset element increment =
  match (InsertionCountSet.find_opt element countset) with
  | None -> InsertionCountSet.add element increment countset
  | Some count -> InsertionCountSet.add element (count + increment) countset
;;

let apply_polymer_step ruleset countset =
  let rec aux (aux_ruleset,aux_countset) bindings =
    match bindings with
    | [] -> aux_ruleset,aux_countset
    | (input_pair, (insertion_element, input_count)) :: rest ->
        if input_count > 0 then
          let new_ruleset = incr_ruleset_pair aux_ruleset input_pair (-1*input_count) in
            let pair1, pair2 = pair_combos input_pair insertion_element in
              let new_ruleset = (incr_ruleset_pair new_ruleset pair1 input_count) in
                let new_ruleset = (incr_ruleset_pair new_ruleset pair2 input_count) in
                  let new_countset = incr_countset aux_countset insertion_element input_count in
                    aux (new_ruleset,new_countset) rest
        else aux (aux_ruleset,aux_countset) rest
  in
    aux (ruleset,countset) (InsertionRuleSet.bindings ruleset)
;;

let apply_n_polymer_steps ruleset countset n =
  let rec aux (ruleset,countset) n =
    match n with
    | 0 -> ruleset,countset
    | _ -> aux (apply_polymer_step ruleset countset) (n-1)
  in
    (aux (ruleset,countset) n)
;;

let populate_ruleset_from_template ruleset template =
  let rec aux ruleset countset template_chars =
    match template_chars with
    | char1 :: char2 :: [] ->
        let pair = [char1;char2] |> List.to_seq |> String.of_seq in
          (incr_ruleset_pair ruleset pair 1),(incr_countset (incr_countset countset char1 1) char2 1)
    | char1 :: char2 :: rest ->
        let pair = [char1;char2] |> List.to_seq |> String.of_seq in
          aux (incr_ruleset_pair ruleset pair 1) (incr_countset countset char1 1) (char2 :: rest)
    | _ -> raise (Failure "unexpected error populating from template")
  in
    let countset = InsertionCountSet.empty in
      aux ruleset countset (template |> String.to_seq |> List.of_seq)
;;

let get_char_min_max countset =
  let sorted_counts = List.sort (fun (_,n1) (_,n2) -> Int.compare n1 n2) (InsertionCountSet.bindings countset) in
    (List.nth sorted_counts 0),(List.nth sorted_counts (List.length sorted_counts - 1))
;;



let string_to_char s =
  List.nth (s |> String.to_seq |> List.of_seq) 0
;;

let parse_insertion_rule_lines lines =
  let ruleset = InsertionRuleSet.empty in
    let rec aux ruleset lines =
      match lines with
      | [] -> ruleset
      | head :: rest ->
          match (Str.split (Str.regexp " -> ") head) with
          | input_pair :: insertion_element :: [] ->
              aux (InsertionRuleSet.add input_pair ((string_to_char insertion_element),0) ruleset) rest
          | _ -> raise (Failure "unexpected insertion rule")
    in
      aux ruleset lines
;;


let maybe_input_line stdin =
  try Some (input_line stdin) with
    End_of_file -> None
;;

let input_lines stdin =
  let rec input lines_a lines_b do_lines_b =
    match maybe_input_line stdin with
    | Some line ->
        if line = ""
          then input lines_a lines_b true
        else if not (do_lines_b)
          then input (line :: lines_a) lines_b do_lines_b
        else input lines_a (line :: lines_b) do_lines_b
    | None -> (List.rev lines_a, List.rev lines_b)
  in
  input [] [] false
;;

let file_channel = open_in("in.txt");;
let template_lines, insertion_rules_lines = input_lines file_channel;;

let polymer_template = List.nth template_lines 0;;
let insertion_ruleset = parse_insertion_rule_lines insertion_rules_lines;;
let insertion_ruleset,countset = populate_ruleset_from_template insertion_ruleset polymer_template;;

let insertion_ruleset,countset = apply_n_polymer_steps insertion_ruleset countset 40;;

let (min_c,min_n), (max_c,max_n) = get_char_min_max countset;;

let () = print_int (max_n-min_n);;
let () = print_newline ();;



let () = close_in file_channel;;
