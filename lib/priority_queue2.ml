type 'a status = Empty | Available of 'a node | Busy of 'a node
and 'a node = { owner : Domain.id option; key : int; item : 'a }

type 'a t = {
  heap_lock : Mutex.t;
  heap : 'a status array;
  fine_lock : Mutex.t array;
  capacity : int;
  next : Brc.t;
}

let create num _ =
  {
    heap_lock = Mutex.create ();
    heap = Array.make num Empty;
    fine_lock = Array.init num (fun _ -> Mutex.create ());
    capacity = num;
    next = Brc.create ();
  }

exception Full

(* add node to the first empty slot and then traverse up to reach correct position *)
let add t item key =
  Mutex.lock t.heap_lock;
  let ind = Brc.increment t.next in
  if ind = t.capacity then (
    (* reached full capacity *)
    Brc.decrement t.next |> ignore;
    Mutex.unlock t.heap_lock;
    Domain.cpu_relax ();
    raise Full)
  else
    let child_ind = ref ind in
    (* insert new node into empty slot *)
    Mutex.lock t.fine_lock.(!child_ind);
    Mutex.unlock t.heap_lock;
    let dom_id = Some (Domain.self ()) in
    t.heap.(!child_ind) <- Busy { owner = dom_id; key; item };
    Mutex.unlock t.fine_lock.(!child_ind);
    (* move up to correct priority position *)
    while !child_ind > 1 do
      let parent_ind = !child_ind / 2 in
      Mutex.lock t.fine_lock.(parent_ind);
      Mutex.lock t.fine_lock.(!child_ind);
      let lock_child_ind = !child_ind in
      let parent = t.heap.(parent_ind) in
      let child = t.heap.(!child_ind) in
      (match (parent, child) with
      | Available parent_node, Busy child_node when child_node.owner = dom_id ->
          if child_node.key < parent_node.key then (
            (* swap if lesser *)
            t.heap.(parent_ind) <- child;
            t.heap.(!child_ind) <- parent;
            child_ind := parent_ind)
          else (
            (* it is in right position *)
            t.heap.(!child_ind) <- Available { child_node with owner = None };
            child_ind := 0 (* to simulate early return *))
      | Empty, _ -> child_ind := 0
      | _, (Empty | Available _) ->
          (* has been moved up because of a remove move down swap *)
          child_ind := parent_ind
      | _, Busy child_node when child_node.owner <> dom_id ->
          child_ind := parent_ind
      | _, _ -> ());
      (* if parent is empty then this node has been moved up by a remove operation *)
      Mutex.unlock t.fine_lock.(lock_child_ind);
      Mutex.unlock t.fine_lock.(parent_ind);
      if !child_ind = lock_child_ind then Domain.cpu_relax ()
    done;

    if !child_ind = 1 then (
      Mutex.lock t.fine_lock.(1);

      let root = t.heap.(1) in
      (match root with
      | Busy node when node.owner = dom_id ->
          t.heap.(1) <- Available { node with owner = None }
      | _ -> ());
      Mutex.unlock t.fine_lock.(1))

(* delete root node and swap with last node, then traverse down to reach correct position *)
let remove_min t =
  Mutex.lock t.heap_lock;
  let bottom_ind = Brc.get_idx t.next in
  match bottom_ind with
  | 0 -> (
      (* return dummy node 42 for timebeing to pass STM tests, check if queue not empty before calling *)
      Mutex.unlock t.heap_lock;
      match t.heap.(0) with Available node -> node.item | _ -> assert false)
  | 1 -> (
      (* Printf.printf "=1 case \n%!"; *)
      ignore @@ Brc.decrement t.next;
      Mutex.lock t.fine_lock.(1);
      Mutex.unlock t.heap_lock;
      match t.heap.(1) with
      | Busy node | Available node ->
          t.heap.(1) <- Empty;
          Mutex.unlock t.fine_lock.(1);
          node.item
      | _ -> assert false)
  | _ -> (
      (* Printf.printf ">1 case \n%!"; *)
      ignore @@ Brc.decrement t.next;
      Mutex.lock t.fine_lock.(1);
      Mutex.lock t.fine_lock.(bottom_ind);
      Mutex.unlock t.heap_lock;
      (* swap bottom and root, set root to empty and bottom to available *)
      let prev = t.heap.(1) in
      match prev with
      | Empty -> assert false
      | Busy root | Available root -> (
          let it = root.item in
          match t.heap.(bottom_ind) with
          | Empty -> assert false
          | Available bottom | Busy bottom ->
              t.heap.(bottom_ind) <- Empty;
              t.heap.(1) <- Available { bottom with owner = None };
              t.heap.(bottom_ind) <- Empty;
              Mutex.unlock t.fine_lock.(bottom_ind);
              let child_ind = ref 0 in
              let par_ind = ref 1 in
              let break = ref false in
              (* simulate breaking out of loop *)
              while !par_ind < t.capacity / 2 && not !break do
                let left = !par_ind * 2 in
                let right = left + 1 in
                Mutex.lock t.fine_lock.(left);
                Mutex.lock t.fine_lock.(right);
                (match (t.heap.(left), t.heap.(right)) with
                | Empty, _ ->
                    Mutex.unlock t.fine_lock.(right);
                    Mutex.unlock t.fine_lock.(left);
                    break := true
                | _, Empty ->
                    Mutex.unlock t.fine_lock.(right);
                    child_ind := left
                | ( (Busy left_node | Available left_node),
                    (Busy right_node | Available right_node) )
                  when left_node.key < right_node.key ->
                    Mutex.unlock t.fine_lock.(right);
                    child_ind := left
                | _, _ ->
                    Mutex.unlock t.fine_lock.(left);
                    child_ind := right);
                if not !break then
                  let child_node = t.heap.(!child_ind) in
                  let par_node = t.heap.(!par_ind) in
                  (* swap par and child if child < par *)
                  match (par_node, child_node) with
                  | Empty, _ | _, Empty -> assert false
                  | ( (Available parent | Busy parent),
                      (Available child | Busy child) ) ->
                      if child.key < parent.key then (
                        t.heap.(!child_ind) <- par_node;
                        t.heap.(!par_ind) <- child_node;
                        Mutex.unlock t.fine_lock.(!par_ind);
                        par_ind := !child_ind)
                      else (
                        (* node is in correct position *)
                        Mutex.unlock t.fine_lock.(!child_ind);
                        break := true)
              done;
              Mutex.unlock t.fine_lock.(!par_ind);
              it))

let is_empty t =
  Mutex.lock t.heap_lock;
  let res = Brc.get_size t.next = 0 in
  Mutex.unlock t.heap_lock;
  res

let get_len t =
  Mutex.lock t.heap_lock;
  let res = Brc.get_size t.next in
  Mutex.unlock t.heap_lock;
  res
