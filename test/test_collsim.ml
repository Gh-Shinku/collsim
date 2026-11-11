open Collsim

let print_ok name =
  Printf.printf "\027[32m[ok]\027[0m %s pass\n" name

let full_input () =
  [|
    { shards = [| Some 1; Some 2; Some 3; Some 4 |] };
    { shards = [| Some 5; Some 6; Some 7; Some 8 |] };
    { shards = [| Some 9; Some 10; Some 11; Some 12 |] };
    { shards = [| Some 13; Some 14; Some 15; Some 16 |] };
  |]

let broadcast_test () =
  let expected = { shards = [| Some 1; Some 2; Some 3; Some 4 |] } in
  let collective =
    [|
      { shards = Array.make 4 None };
      { shards = Array.make 4 None };
      expected;
      { shards = Array.make 4 None };
    |]
  in
  let res = broadcast collective 2 in
  assert (Array.for_all (fun rank -> rank = expected) res);
  print_ok "broadcast_test"

let scatter_test () =
  let collective =
    [|
      { shards = Array.make 4 None };
      { shards = Array.make 4 None };
      { shards = [| Some 1; Some 2; Some 3; Some 4 |] };
      { shards = Array.make 4 None };
    |]
  in
  let res = scatter collective 2 [| 0; 1; 2; 3 |] in
  let expected =
    [|
      { shards = [| Some 1; None; None; None |] };
      { shards = [| None; Some 2; None; None |] };
      { shards = [| None; None; Some 3; None |] };
      { shards = [| None; None; None; Some 4 |] };
    |]
  in
  assert (res = expected);
  let res = scatter collective 2 [| 2; 0; 3; 1 |] in
  let expected =
    [|
      { shards = [| None; Some 2; None; None |] };
      { shards = [| None; None; None; Some 4 |] };
      { shards = [| Some 1; None; None; None |] };
      { shards = [| None; None; Some 3; None |] };
    |]
  in
  assert (res = expected);
  print_ok "scatter_test"

let gather_test () =
  let collective =
    [|
      { shards = [| None; None; Some 1; None |] };
      { shards = [| None; None; None; Some 2 |] };
      { shards = [| None; Some 3; None; None |] };
      { shards = [| Some 4; None; None; None |] };
    |]
  in
  let res = gather collective 2 [| 2; 3; 1; 0 |] in
  let expected =
    [|
      { shards = Array.make 4 None };
      { shards = Array.make 4 None };
      { shards = [| Some 4; Some 3; Some 1; Some 2 |] };
      { shards = Array.make 4 None };
    |]
  in
  assert (res = expected);
  print_ok "gather_test"

let reduce_test () =
  let res = reduce (full_input ()) 2 ( + ) 0 in
  let expected =
    [|
      { shards = Array.make 4 None };
      { shards = Array.make 4 None };
      { shards = [| Some 28; Some 32; Some 36; Some 40 |] };
      { shards = Array.make 4 None };
    |]
  in
  assert (res = expected);
  let collective =
    [|
      { shards = [| Some 2; Some 3 |] };
      { shards = [| Some 5; Some 7 |] };
      { shards = [| Some 11; Some 13 |] };
    |]
  in
  let res = reduce collective 1 ( * ) 1 in
  let expected =
    [|
      { shards = [| None; None |] };
      { shards = [| Some 110; Some 273 |] };
      { shards = [| None; None |] };
    |]
  in
  assert (res = expected);
  print_ok "reduce_test"

let reduce_scatter_test () =
  let collective = full_input () in
  let res = reduce_scatter collective [| 0; 1; 2; 3 |] ( + ) 0 in
  let expected =
    [|
      { shards = [| Some 28; None; None; None |] };
      { shards = [| None; Some 32; None; None |] };
      { shards = [| None; None; Some 36; None |] };
      { shards = [| None; None; None; Some 40 |] };
    |]
  in
  assert (res = expected);
  let res = reduce_scatter collective [| 3; 0; 1; 2 |] ( + ) 0 in
  let expected =
    [|
      { shards = [| None; None; None; Some 40 |] };
      { shards = [| Some 28; None; None; None |] };
      { shards = [| None; Some 32; None; None |] };
      { shards = [| None; None; Some 36; None |] };
    |]
  in
  assert (res = expected);
  print_ok "reduce_scatter_test"

let all_gather_test () =
  let collective =
    [|
      { shards = [| None; None; Some 1; None |] };
      { shards = [| None; None; None; Some 2 |] };
      { shards = [| None; Some 3; None; None |] };
      { shards = [| Some 4; None; None; None |] };
    |]
  in
  let res = all_gather collective [| 2; 3; 1; 0 |] in
  let expected = { shards = [| Some 4; Some 3; Some 1; Some 2 |] } in
  assert (Array.for_all (fun rank -> rank = expected) res);
  print_ok "all_gather_test"

let all_reduce_test () =
  let collective = full_input () in
  let expected = { shards = [| Some 28; Some 32; Some 36; Some 40 |] } in
  let res = all_reduce collective [| 0; 1; 2; 3 |] ( + ) 0 in
  assert (Array.for_all (fun rank -> rank = expected) res);
  let res = all_reduce collective [| 3; 0; 1; 2 |] ( + ) 0 in
  assert (Array.for_all (fun rank -> rank = expected) res);
  assert (
    all_reduce [| { shards = [| Some 7 |] } |] [| 0 |] ( + ) 0
    = [| { shards = [| Some 7 |] } |]);
  print_ok "all_reduce_test"

let all_to_all_test () =
  let res = all_to_all (full_input ()) in
  let expected =
    [|
      { shards = [| Some 1; Some 5; Some 9; Some 13 |] };
      { shards = [| Some 2; Some 6; Some 10; Some 14 |] };
      { shards = [| Some 3; Some 7; Some 11; Some 15 |] };
      { shards = [| Some 4; Some 8; Some 12; Some 16 |] };
    |]
  in
  assert (res = expected);
  print_ok "all_to_all_test"

let () =
  broadcast_test ();
  scatter_test ();
  gather_test ();
  reduce_test ();
  reduce_scatter_test ();
  all_gather_test ();
  all_reduce_test ();
  all_to_all_test ()
