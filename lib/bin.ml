(*let test n nd =
  let barrier = Atomic.make nd in
  let q = init () in

  let work n lpush =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do
      Domain.cpu_relax ()
    done;

    let rec loop count lpush acc =
      match (count, Random.int 2, lpush) with
      | 0, _, _ -> List.rev acc
      | _, 0, x :: xs ->
          push q x;
          Domain.cpu_relax ();
          loop (count - 1) xs (`Push x :: acc)
      | _, _, _ ->
          let popped = pop q in
          Domain.cpu_relax ();
          loop (count - 1) lpush (`Pop popped :: acc)
    in
    loop n lpush []
  in

  let array_lpush =
    Array.init nd (fun id -> List.init n (fun i -> i + (n * id)))
  in

  let domains =
    Array.init nd (fun id -> Domain.spawn (fun () -> work n array_lpush.(id)))
  in
  Array.map Domain.join domains

let test n =
  let nd = 2 in
  let barrier = Atomic.make nd in
  let q = init () in

  let work n lpush =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do
      Domain.cpu_relax ()
    done;

    let rec loop count lpush acc =
      match (count, Random.int 2, lpush) with
      | 0, _, _ -> List.rev acc
      | _, 0, x :: xs ->
          push q x;
          Domain.cpu_relax ();
          loop (count - 1) xs (`Push x :: acc)
      | _, _, _ ->
          let popped = pop q in
          Domain.cpu_relax ();
          loop (count - 1) lpush (`Pop popped :: acc)
    in
    loop n lpush []
  in

  let array_lpush =
    Array.init nd (fun id -> List.init n (fun i -> i + (n * id)))
  in

  let domains =
    Array.init nd (fun id -> Domain.spawn (fun () -> work n array_lpush.(id)))
  in
  let res = Array.map Domain.join domains |> Array.to_list in
  match res with [ r1; r2 ] -> (r1, r2) | _ -> assert false

let rec check ?(print=false) r1 r2 model =
  match (r1, r2, List.rev model) with
  | [], [], _ ->
      if print then Format.printf "1@.";
      true
  | `Push a1 :: xs1, `Push a2 :: xs2, _ ->
      if print then Format.printf "2@.";
      check xs1 r2 (a1 :: model) || check r1 xs2 (a2 :: model)
  | `Pop (Some v1) :: xs1, _, m :: model when m = v1 ->
      if print then Format.printf "3@.";
      check xs1 r2 (List.rev model)
  | _, `Pop (Some v2) :: xs2, m :: model when m = v2 ->
      if print then Format.printf "4@.";
      check r1 xs2 (List.rev model)
  | `Push a :: xs, `Pop None :: _, x :: _ ->
      if print then Format.printf "5@.";
      check xs r2 (a :: model)
  | `Pop None :: _, `Push a :: xs, x :: _ ->
      if print then Format.printf "6@.";
      check r1 xs (a :: model)
  | `Push a :: xs, `Pop (Some _) :: _, _ ->
      if print then Format.printf "7@.";
      check xs r2 (a :: model)
  | `Pop (Some _) :: _, `Push a :: xs, _ ->
      if print then Format.printf "8@.";
      check r1 xs (a :: model)
  | `Push a1 :: xs1, `Pop None :: xs2, [] ->
      if print then Format.printf "9@.";
      check xs1 r2 [ a1 ] || check r1 xs2 []
  | `Pop None :: xs1, `Push a2 :: xs2, [] ->
      if print then Format.printf "10@.";
      check xs1 r2 [] || check r1 xs2 [ a2 ]
  | [], `Pop (Some _) :: xs, [] | `Pop (Some _) :: xs, [], [] ->
      if print then Format.printf "11@.";
      false
  | [], `Push a :: xs, _ | `Push a :: xs, [], _ ->
      if print then Format.printf "12@.";
      check xs [] (a :: model)
  | [], `Pop None :: xs, [] | `Pop None :: xs, [], [] ->
      if print then Format.printf "13@.";
      check xs [] model
  | [], `Pop None :: _, _ | `Pop None :: _, [], _ ->
      if print then Format.printf "14@.";
      false
  | [], `Pop (Some a) :: xs, _ | `Pop (Some a) :: xs, [], _ ->
      if print then Format.printf "15@.";
      check xs [] (a :: model)
  | `Pop None :: xs1, `Pop None :: xs2, [] ->
      if print then Format.printf "16@.";
      check xs1 r2 [] || check r1 xs2 []
  | `Pop _ :: _, `Pop None :: xs2, [] ->
      if print then Format.printf "17@.";
      check r1 xs2 []
  | `Pop None :: xs1, `Pop _ :: _, [] ->
      if print then Format.printf "18@.";
      check xs1 r2 []
  | `Pop _ :: _, `Pop _ :: _, _ ->
      if print then Format.printf "19@.";
      false

let main () =
  let count = ref 10000 in
  let b = ref true in
  while !b && !count > 0 do
    let _r1, _r2 = test 10 in
    b := check _r1 _r2 [];
    if not !b then failwith "Not working";
    decr count
  done
*)
