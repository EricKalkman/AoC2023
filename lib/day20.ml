open Common
open Parsing

type pulse_kind = Low | High
type ff_state = Off | On
type pulse = { src : string; dst : string; kind : pulse_kind }
type aocmodule = FlipFlop | Conj | Broadcast | Dummy

let neg s = match s with Off -> On | _ -> Off
let expect_name = expect_lower |> at_least_one |> group >=> stringify_top

let broadcaster_parser =
  expect_string "broadcaster"
  >=> skip_string " -> "
  >=> expect_list expect_name (expect_string ", ")

let flipflop_parser =
  expect_char '%' >=> expect_name >=> skip_string " -> "
  >=> expect_list expect_name (expect_string ", ")

let conj_parser =
  expect_char '&' >=> expect_name >=> skip_string " -> "
  >=> expect_list expect_name (expect_string ", ")

let line_parser = broadcaster_parser >=>? flipflop_parser >=>? conj_parser

let process_module = function
  | PString "broadcaster" :: dests ->
      ("broadcaster", (Broadcast, List.map unwrap_ps dests))
  | Char '%' :: PString name :: dests ->
      (name, (FlipFlop, List.map unwrap_ps dests))
  | Char '&' :: PString name :: dests -> (name, (Conj, List.map unwrap_ps dests))
  | _ -> failwith "invalid input format"

module M = Map.Make (String)
module SS = Set.Make (String)

let process_input inp =
  let modules =
    inp
    |> List.map
         (run_string_parser line_parser >> unwrap_result >> process_module)
  in
  let mod_map = M.of_list modules in
  (* handle modules that don't have any outgoing edges*)
  let dummies =
    SS.diff
      (M.to_seq mod_map |> Seq.flat_map (snd >> snd >> List.to_seq) |> SS.of_seq)
      (M.to_seq mod_map |> Seq.map fst |> SS.of_seq)
    |> SS.to_seq
    |> Seq.map (fun name -> (name, (Dummy, [])))
    |> M.of_seq
  in
  ( Seq.append (M.to_seq mod_map) (M.to_seq dummies) |> M.of_seq,
    (* find all nodes that feed into conjunctions *)
    List.to_seq modules
    |> Seq.filter (fun (_, (kind, _)) -> kind == Conj)
    |> Seq.map fst
    |> Seq.map (fun name ->
           ( name,
             List.(
               modules
               |> find_all (fun (_, (_, dests)) -> List.mem name dests)
               |> map (fun x -> (fst x, Low))
               |> M.of_list) ))
    |> M.of_seq,
    (* set up the states for the flip-flops *)
    List.to_seq modules
    |> Seq.filter (fun (_, (kind, _)) -> kind == FlipFlop)
    |> Seq.map (fun x -> (fst x, Off))
    |> M.of_seq )

let process_pulse q mods conjs ffs =
  (* takes a queue of pulses, the modules' names mapped to their kind/destination,
     the memory for the Conjuctions, and the states of the Flip-Flops *)
  (* pops the first pulse of the queue and processes it *)
  (* adds any new pulses to the queue, and updates memory/state appropriately *)
  match q with
  | [] -> (q, conjs, ffs)
  | p :: q -> (
      match M.find p.dst mods with
      | Broadcast, dests ->
          ( List.append q
              List.(
                dests
                |> map (fun d ->
                       { src = "broadcaster"; dst = d; kind = p.kind })),
            conjs,
            ffs )
      | FlipFlop, dests -> (
          match p.kind with
          | High -> (q, conjs, ffs)
          | Low ->
              let next_state = neg @@ M.find p.dst ffs in
              let next_p_kind =
                match next_state with On -> High | Off -> Low
              in
              ( (List.append q
                @@ List.(
                     dests
                     |> map (fun d ->
                            { src = p.dst; dst = d; kind = next_p_kind }))),
                conjs,
                M.add p.dst next_state ffs ))
      | Conj, dests ->
          let conjs =
            update_unsafe M.update p.dst (fun m -> M.add p.src p.kind m) conjs
          in
          let next_kind =
            if
              conjs |> M.find p.dst |> M.to_seq |> Seq.map snd
              |> all (( == ) High)
            then Low
            else High
          in
          ( (List.append q
            @@ List.(
                 dests
                 |> map (fun d -> { src = p.dst; dst = d; kind = next_kind }))),
            conjs,
            ffs )
      | Dummy, _ -> (q, conjs, ffs))

let push_button mods conjs ffs =
  (* simulate pushing the button. returns the (reversed) list of pulses
     as well as the updated Conjunctions' and Flip-Flops' states *)
  let rec push_button' q conjs ffs pulse_list =
    (* keep popping pulses from the queue until there are none left *)
    match q with
    | [] -> (pulse_list, conjs, ffs)
    | p :: q ->
        let q, conjs, ffs = process_pulse (p :: q) mods conjs ffs in
        push_button' q conjs ffs @@ List.cons p pulse_list
  in
  push_button'
    [ { src = "button"; dst = "broadcaster"; kind = Low } ]
    conjs ffs []

let part_1 inp =
  let mods, conjs, ffs = process_input inp in
  let num_presses = 1000 in
  (* mash the button, keeping track of the sequence of pulses *)
  Seq.unfold
    (fun (conjs, ffs) ->
      let pulses, conjs, ffs = push_button mods conjs ffs in
      Some (pulses, (conjs, ffs)))
    (conjs, ffs)
  (* but only do it num_presses times *)
  |> Seq.take num_presses
  (* we only care about the total numbers of each pulse; extract them *)
  |> Seq.flat_map (fun pulses ->
         Seq.(List.to_seq pulses |> map (fun p -> p.kind)))
  (* split them between low and high *)
  |> Seq.partition (( == ) Low)
  |> fun (lows, highs) -> Seq.length lows * Seq.length highs

let sources_of_node node mods =
  M.to_seq mods
  |> Seq.filter (fun (_, (_, dests)) -> List.mem node dests)
  |> Seq.map fst

let rec gcd a b =
  if a < b then gcd b a else if b == 0 then a else gcd b (a mod b)

let lcm a b = a * b / gcd a b

let part_2 inp =
  let mods, conjs, ffs = process_input inp in
  (* rx has exactly one source module that happens to be a Conjunction *)
  let rx_src = sources_of_node "rx" mods |> List.of_seq |> List.hd in
  (* rx's source has several modules *)
  let num_rx_src_srcs = sources_of_node rx_src mods |> Seq.length in
  (* the algorithm figures out when each of the Conjunction's srcs
     sends a high pulse, which is the only condition under which rx
     would receive a low pulse *)
  (* solve keeps track of the number of button presses n as well as
     the press during which the first instance of each of rx's sources'
     sources sending a high pulse *)
  (* assumes that there is no phase offset, which is true for my input *)
  let rec solve n srcs conjs ffs =
    let pulses, conjs, ffs = push_button mods conjs ffs in
    let srcs =
      (* find all pulses that lit up rx's source with a high pulse, if any *)
      List.find_all
        (fun p -> String.equal p.dst rx_src && p.kind == High)
        pulses
      (* record them as having been pressed during this button press *)
      |> List.fold_left
           (fun srcs p ->
             match M.find_opt p.src srcs with
             | None -> M.add p.src n srcs
             (* already seen this one light up; don't update *)
             | Some _ -> srcs)
           srcs
    in
    if M.cardinal srcs == num_rx_src_srcs then srcs
    else solve (n + 1) srcs conjs ffs
  in
  solve 1 M.empty conjs ffs |> M.to_seq |> Seq.map snd |> Seq.fold_left lcm 1
