open MulticamlDS

module type LLIST = sig
  type 'a t

  val create : 'a -> 'a t
  val add : 'a t -> 'a -> bool
  val remove : 'a t -> 'a -> bool
  val is_empty : 'a t -> bool
end

let test name (module Llist : LLIST) =
  for num_domains = 1 to 8 do
    let len = 20_000 / num_domains in

    (*amount of work by each domain*)
    let glock = Mutex.create () in
    let list = Llist.create 42 in
    for _ = 1 to 10_000 do
      let relt = Random.int 30_000_000 in
      ignore @@ Llist.add list relt
    done;

    let start_time = Unix.gettimeofday () in

    let dom_list =
      Array.init num_domains (fun _ ->
          Domain.spawn (fun () ->
              for _ = 1 to len do
                let relt = Random.int 30_000_000 in
                Mutex.lock glock;
                ignore @@ Llist.add list relt;
                Mutex.unlock glock
              done))
    in
    for i = 0 to num_domains - 1 do
      Domain.join dom_list.(i)
    done;

    let end_time = Unix.gettimeofday () in
    Format.printf "%s - Completed %d domains in %f@." name num_domains
      (end_time -. start_time)
  done

let _ =
  Random.self_init ();
  let all_lists =
    [
      ("Basic list", (module Basic_list : LLIST));
      ("fine_list", (module Fine_list : LLIST));
    ]
  in
  List.iter (fun (name, (module L : LLIST)) -> test name (module L)) all_lists
