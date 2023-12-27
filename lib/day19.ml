open Common
open Parsing

(* parser productions; these are just the tokenizers *)
(* the actual logic starts at line 101 lol *)
let node_parser = expect_lower |> at_least_one |> group >=> stringify_top
let result_parser = node_parser >=>? expect_char 'A' >=>? expect_char 'R'
let expr_parser = node_parser >=> expect_set "<>" >=> expect_int
let cond_parser = expr_parser >=> skip_char ':' >=> result_parser |> group

let workflow_parser =
  node_parser >=> skip_char '{'
  >=> expect_nonempty_list cond_parser (expect_char ',')
  >=> skip_char ',' >=> result_parser >=> skip_char '}' |> group

let workflows_parser = expect_list workflow_parser skip_nl |> group
let term_parser = node_parser >=> skip_char '=' >=> expect_int |> group

let binding_parser =
  skip_char '{'
  >=> expect_list term_parser (expect_char ',')
  >=> skip_char '}' |> group

let bindings_parser = expect_list binding_parser skip_nl |> group
let input_parser = workflows_parser >=> skip_nl >=> skip_nl >=> bindings_parser

(* types to represent the workflows *)
type node = string
type decision = Accept | Reject
type op = LT | GT

(* result can be an accept/reject decision or another workflow (node) *)
type result = Node of node | Decision of decision

(* conditional phrase: reg `op` cnst -> res; reg = "register", i.e., x, m, a, or s *)
type cond = { op : op; reg : node; cnst : int; res : result }

(* a workflow has a final "else" branch as well as a series of conditional phrases *)
type workflow = { final : result; conds : cond list }

(* glorious functional parsing *)
let op_of_char = function
  | '<' -> LT
  | '>' -> GT
  | _ -> invalid_arg "op_of_char"

let process_result = function
  | Char 'A' -> Decision Accept
  | Char 'R' -> Decision Reject
  | PString x -> Node x
  | _ -> invalid_arg "process_result"

let process_cond cond =
  match cond |> unwrap_group with
  | [ PString name; Char c; Int x; res ] ->
      { op = op_of_char c; reg = name; cnst = x; res = process_result res }
  | _ -> invalid_arg "process_cond"

let process_workflow g =
  match g |> unwrap_group with
  | PString name :: rest -> (
      (* the last item in rest is the target of the last else branch *)
      match List.rev rest with
      | final :: conds ->
          ( name,
            {
              final = process_result final;
              conds = List.(rev conds |> map process_cond);
            } )
      | _ -> invalid_arg "failed to match workflow format: final")
  | _ -> invalid_arg "failed to match workflow format: head"

(* settings for the XMAS system (second half of input) *)
type binding = { x : int; m : int; a : int; s : int }

let process_binding g =
  match unwrap_group g with
  | [
   Group [ PString "x"; Int x ];
   Group [ PString "m"; Int m ];
   Group [ PString "a"; Int a ];
   Group [ PString "s"; Int s ];
  ] ->
      { x; m; a; s }
  | _ -> invalid_arg "process_binding"

let index_binding reg binding =
  match reg with
  | "x" -> binding.x
  | "m" -> binding.m
  | "a" -> binding.a
  | "s" -> binding.s
  | _ -> invalid_arg ("invalid XMAS code " ^ reg)

module M = Map.Make (String)

let process_input inp =
  match run_string_parser input_parser inp |> unwrap_result with
  | [ Group workflows; Group bindings ] ->
      ( List.map process_workflow workflows |> M.of_list,
        List.map process_binding bindings )
  | _ -> invalid_arg "failed to match two components"

let eval_cond bind cond =
  (* evaluates a conditional clause given the XMAS settings in bind *)
  match cond.op with
  | LT -> index_binding cond.reg bind < cond.cnst
  | GT -> index_binding cond.reg bind > cond.cnst

let rec eval_at wfs bind wf_name =
  (* why do all the parsing above? so that this function can be stupidly
     concise *)
  let wf = M.find wf_name wfs in
  match
    wf.conds |> List.to_seq
    (* find the first successful conditional *)
    |> Seq.drop_while (eval_cond bind >> not)
    |> Seq.uncons
    (* if there was no successful condition, return wf.final; else, return
       the branch destination of the conditinoal (cond.res) *)
    |> Option.fold ~none:wf.final ~some:(fun (cond, _) -> cond.res)
  with
  | Decision x -> x
  (* workflow goes to another workflow; recurse *)
  | Node name -> eval_at wfs bind name

let part_1 inp =
  let wfs, binds = process_input inp in
  binds
  |> List.filter (fun bind -> eval_at wfs bind "in" == Accept)
  |> List.fold_left (fun acc bind -> acc + bind.x + bind.m + bind.a + bind.s) 0

let func_of_cond cond x =
  match cond.op with LT -> x < cond.cnst | GT -> x > cond.cnst

let count_paths wfs =
  (* decidedly less concise than part 1 *)
  (* builds One Giant Lambda to filter on the register ranges (1 .. 4000) while
     traversing the workflow tree from its root. Each edge from a parent to a
     child represents traversing the conditional. As such, a lambda that filters
     according to the conditional is inductively &&'d onto the filter function for
     the appropriate XMAS register at each step. When a leaf is reached, the filter
     functions are applied over the sequence of ints 1 .. 4000, the final counts of
     which are returned back up the tree. *)
  let rec count_paths' filterfuncs res =
    match res with
    | Decision Reject -> 0
    | Decision Accept ->
        (* we've built the filter function, now apply it *)
        let max_rng = 4000 in
        let inp_rng = Seq.ints 1 |> Seq.take max_rng in
        let counter l =
          inp_rng |> Seq.filter (M.find l filterfuncs) |> Seq.length
        in
        (* each register is independent, so the number of options stack
           multiplicatively *)
        counter "x" * counter "m" * counter "a" * counter "s"
    | Node name ->
        (* we've been sent to another workflow *)
        let wf = M.find name wfs in
        let counts, elsefuncs =
          (* for each conditional of the current workflow *)
          wf.conds
          |> List.fold_left
               (fun (count, filters) cond ->
                 (* function that screens register values according to the
                    conditional described by cond *)
                 let condfunc = func_of_cond cond in
                 let cur_filterfuncs =
                   update_unsafe M.update cond.reg
                     (* all preceding conditionals must succeed, as must the current one *)
                       (fun filterfunc x -> filterfunc x && condfunc x)
                     filters
                 in
                 let cur_counts = count_paths' cur_filterfuncs cond.res in
                 (* now consider the branch where the conditional failed. this branch
                    filters on not (condfunc x) *)
                 let next_filterfuncs =
                   update_unsafe M.update cond.reg
                     (fun filterfunc x -> filterfunc x && not (condfunc x))
                     filters
                 in
                 (* counts scale additively here because we are partitioning the count
                    among the edges of our tree *)
                 (count + cur_counts, next_filterfuncs))
               (0, filterfuncs)
        in
        (* consider the case where all of the conditionals are false *)
        let elsecount = count_paths' elsefuncs wf.final in
        counts + elsecount
  in
  let truefn _ = true in
  let filters =
    [ ("x", truefn); ("m", truefn); ("a", truefn); ("s", truefn) ] |> M.of_list
  in
  count_paths' filters (Node "in")

let part_2 inp =
  let wfs, _ = process_input inp in
  count_paths wfs
