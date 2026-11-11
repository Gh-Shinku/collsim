open Collsim

let print_collective name collective =
  print_endline name;
  Array.iteri
    (fun rank state ->
      let shards =
        Array.map
          (function None -> "None" | Some value -> string_of_int value)
          state.shards
      in
      Printf.printf "  rank %d: [%s]\n" rank
        (String.concat "; " (Array.to_list shards)))
    collective

let () =
  let root = 2 in
  let initial =
    Array.init 4 (fun rank ->
        {
          shards =
            (if rank = root then [| Some 1; Some 2; Some 3; Some 4 |]
             else Array.make 4 None);
        })
  in
  print_collective "Input (root = 2)" initial;
  print_collective "Broadcast" (broadcast initial root);
  print_collective "Scatter" (scatter initial root [| 0; 1; 2; 3 |])
