type 'a rank = { shards : 'a option array }
type 'a collective = 'a rank array
type 'a message = { src : int; dst : int; shard : int; value : 'a }
type int_mapping = int array

let reverse_int_mapping (mapping : int_mapping) : int_mapping =
  let new_mapping = Array.make (Array.length mapping) 0 in
  Array.iteri (fun idx x -> new_mapping.(x) <- idx) mapping;
  new_mapping

let copy_state (collective : 'a collective) : 'a collective =
  Array.map (fun rank -> { shards = Array.copy rank.shards }) collective

let empty_state (collective : 'a collective) : 'a collective =
  Array.map
    (fun rank -> { shards = Array.make (Array.length rank.shards) None })
    collective

let deliver (collective : 'a collective) (messages : 'a message array) :
    'a collective =
  let next = copy_state collective in
  Array.iter
    (fun message ->
      next.(message.dst).shards.(message.shard) <- Some message.value)
    messages;
  next

let broadcast (collective : 'a collective) (root : int) : 'a collective =
  let p = Array.length collective in
  let send_buf =
    Array.mapi
      (fun shard value ->
        { src = root; dst = (root + 1) mod p; shard; value = Option.get value })
      collective.(root).shards
  in
  let rec rounds remaining current send_buf =
    if remaining = 0 then current
    else
      let next = deliver current send_buf in
      let next_send_buf =
        Array.map
          (fun message ->
            { message with src = message.dst; dst = (message.dst + 1) mod p })
          send_buf
      in
      rounds (remaining - 1) next next_send_buf
  in
  rounds (p - 1) (copy_state collective) send_buf

let scatter (collective : 'a collective) (root : int) (shard2rank : int_mapping)
    : 'a collective =
  let messages =
    Array.mapi
      (fun shard dst ->
        {
          src = root;
          dst;
          shard;
          value = Option.get collective.(root).shards.(shard);
        })
      shard2rank
  in
  deliver (empty_state collective) messages

let gather (collective : 'a collective) (root : int) (rank2shard : int_mapping)
    : 'a collective =
  let messages =
    Array.mapi
      (fun src shard ->
        {
          src;
          dst = root;
          shard;
          value = Option.get collective.(src).shards.(shard);
        })
      rank2shard
  in
  deliver (empty_state collective) messages

let reduce (collective : 'a collective) (root : int)
    (reduce_op : 'a -> 'a -> 'a) (identity : 'a) : 'a collective =
  let send_buf =
    Array.mapi
      (fun src rank ->
        Array.mapi
          (fun shard value ->
            { src; dst = root; shard; value = Option.get value })
          rank.shards)
      collective
  in
  let result = empty_state collective in
  Array.fill result.(root).shards 0
    (Array.length result.(root).shards)
    (Some identity);
  Array.iter
    (fun messages ->
      Array.iter
        (fun message ->
          let current =
            Option.get result.(message.dst).shards.(message.shard)
          in
          result.(message.dst).shards.(message.shard) <-
            Some (reduce_op current message.value))
        messages)
    send_buf;
  result

(* rank2shard is a permutation: rank r keeps the result for rank2shard.(r).
   The cyclic reduction order requires an associative, commutative operation. *)
let reduce_scatter (collective : 'a collective) (rank2shard : int_mapping)
    (reduce_op : 'a -> 'a -> 'a) (identity : 'a) : 'a collective =
  let p = Array.length collective in
  (* Start each shard at the owner's right neighbor. After p - 1 hops,
     its partial reduction reaches the owner and includes every rank. *)
  let send_buf =
    Array.mapi
      (fun owner shard ->
        let src = (owner + 1) mod p in
        {
          src;
          dst = (src + 1) mod p;
          shard;
          value =
            reduce_op identity (Option.get collective.(src).shards.(shard));
        })
      rank2shard
  in
  let initial = copy_state collective in
  Array.iter
    (fun message ->
      initial.(message.src).shards.(message.shard) <- Some message.value)
    send_buf;
  let rec rounds remaining current send_buf =
    if remaining = 0 then current
    else
      (* Each receiver combines the incoming partial value with its own
         contribution. This shard has not visited that receiver before. *)
      let recv_buf =
        Array.map
          (fun message ->
            let local =
              Option.get current.(message.dst).shards.(message.shard)
            in
            { message with value = reduce_op message.value local })
          send_buf
      in
      let next = deliver current recv_buf in
      let next_send_buf =
        Array.map
          (fun message ->
            { message with src = message.dst; dst = (message.dst + 1) mod p })
          recv_buf
      in
      rounds (remaining - 1) next next_send_buf
  in
  let reduced = rounds (p - 1) initial send_buf in
  let result = empty_state collective in
  Array.iteri
    (fun rank shard ->
      result.(rank).shards.(shard) <- reduced.(rank).shards.(shard))
    rank2shard;
  result

let all_gather (collective : 'a collective) (rank2shard : int_mapping) :
    'a collective =
  let p = Array.length collective in
  let send_buf =
    Array.mapi
      (fun src shard ->
        {
          src;
          dst = (src + 1) mod p;
          shard;
          value = Option.get collective.(src).shards.(shard);
        })
      rank2shard
  in
  let rec rounds remaining current send_buf =
    if remaining = 0 then current
    else
      let next = deliver current send_buf in
      let next_send_buf =
        Array.map
          (fun message ->
            { message with src = message.dst; dst = (message.dst + 1) mod p })
          send_buf
      in
      rounds (remaining - 1) next next_send_buf
  in
  rounds (p - 1) (copy_state collective) send_buf

let all_reduce (collective : 'a collective) (rank2shard : int_mapping)
    (reduce_op : 'a -> 'a -> 'a) (identity : 'a) : 'a collective =
  let scattered = reduce_scatter collective rank2shard reduce_op identity in
  all_gather scattered rank2shard

(* Input shards are indexed by destination; output shards are indexed by source. *)
let all_to_all (collective : 'a collective) : 'a collective =
  let send_buf =
    Array.mapi
      (fun src rank ->
        Array.mapi
          (fun dst value -> { src; dst; shard = src; value = Option.get value })
          rank.shards)
      collective
  in
  let messages = Array.concat (Array.to_list send_buf) in
  deliver (empty_state collective) messages
