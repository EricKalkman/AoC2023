open Common
open Parsing

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

let setting_parser =
  skip_char '{'
  >=> expect_list term_parser (expect_char ',')
  >=> skip_char '}' |> group

let settings_parser = expect_list setting_parser skip_nl |> group
let input_parser = workflows_parser >=> skip_nl >=> skip_nl >=> settings_parser

type node = string
type decision = Accept | Reject
type op = LT | GT
type result = Node of node | Decision of decision
type cond = { op : op; node : node; cnst : int; res : result }
type workflow = { final : result; conds : cond list }

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
      { op = op_of_char c; node = name; cnst = x; res = process_result res }
  | _ -> invalid_arg "process_cond"

let process_workflow g =
  match g |> unwrap_group with
  | PString name :: rest -> (
      match List.rev rest with
      | res :: conds ->
          ( name,
            {
              final = process_result res;
              conds = List.(rev conds |> map process_cond);
            } )
      | _ -> invalid_arg "failed to match workflow format: final")
  | _ -> invalid_arg "failed to match workflow format: head"

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

let index_binding x binding =
  match x with
  | "x" -> binding.x
  | "m" -> binding.m
  | "a" -> binding.a
  | "s" -> binding.s
  | _ -> invalid_arg ("invalid XMAS code " ^ x)

module M = Map.Make (String)

let process_input inp =
  match run_string_parser input_parser inp |> unwrap_result with
  | [ Group workflows; Group bindings ] ->
      ( List.map process_workflow workflows |> M.of_list,
        List.map process_binding bindings )
  | _ -> invalid_arg "failed to match two components"

let eval_cond binds cond =
  match cond.op with
  | LT -> index_binding cond.node binds < cond.cnst
  | GT -> index_binding cond.node binds > cond.cnst

let rec eval_at wfs bind wf_name =
  let wf = M.find wf_name wfs in
  match
    wf.conds |> List.to_seq
    |> Seq.drop_while (eval_cond bind >> not)
    |> Seq.uncons
    |> Option.fold ~none:wf.final ~some:(fun (cond, _) -> cond.res)
  with
  | Decision x -> x
  | Node name -> eval_at wfs bind name

let part_1 inp =
  let wfs, binds = process_input inp in
  binds
  |> List.filter (fun bind -> eval_at wfs bind "in" == Accept)
  |> List.fold_left (fun acc bind -> acc + bind.x + bind.m + bind.a + bind.s) 0

let func_of_cond cond =
  match cond.op with
  | LT -> fun x -> x < cond.cnst
  | GT -> fun x -> x > cond.cnst

let count_paths wfs =
  let rec count_paths' filterfuncs res =
    match res with
    | Decision Reject -> 0
    | Decision Accept ->
        let max_rng = 4000 in
        let inp_rng = Seq.ints 1 |> Seq.take max_rng in
        let counter l =
          inp_rng |> Seq.filter (M.find l filterfuncs) |> Seq.length
        in
        counter "x" * counter "m" * counter "a" * counter "s"
    | Node name ->
        let wf = M.find name wfs in
        let counts, elsefuncs =
          wf.conds
          |> List.fold_left
               (fun (count, filters) cond ->
                 let condfunc = func_of_cond cond in
                 let cur_filterfuncs =
                   update_unsafe M.update cond.node
                     (fun conjnot x -> conjnot x && condfunc x)
                     filters
                 in
                 let cur_counts = count_paths' cur_filterfuncs cond.res in
                 let next_filterfuncs =
                   update_unsafe M.update cond.node
                     (fun conjnot x -> conjnot x && not (condfunc x))
                     filters
                 in
                 (count + cur_counts, next_filterfuncs))
               (0, filterfuncs)
        in
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
