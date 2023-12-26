import strutils
import sequtils
import critbits
import sets

proc process_input: CritBitTree[HashSet[string]] =
  for ln in lines(stdin):
    let spt1 = ln.split(": ")
    let src = spt1[0]
    let dsts = spt1[1].split(" ")
    for dst in dsts:
      if result.containsOrIncl(src, [dst].toHashSet):
        result[src].incl dst
      if result.containsOrIncl(dst, [src].toHashSet):
        result[dst].incl src

func outside_connectivity(edges: CritBitTree[HashSet[string]]; verts: HashSet[string]; v: string): int =
  (edges[v] - verts).card

func solve(edges: CritBitTree[HashSet[string]]): HashSet[string] =
  var verts = edges.keys.toSeq.toHashSet
  while true:
    let connectivities = mapIt(verts, (it, outside_connectivity(edges, verts, it)))
    if foldl(connectivities, a + b[1], 0) == 3:
      return verts
    else:
      let mx = foldl(connectivities, if b[1] > a[1]: b else: a, ("invalid", int.low))
      verts.excl mx[0]

when isMainModule:
  proc main =
    let all_edges = process_input()
    let s = solve all_edges
    echo s.len * (all_edges.len - s.len)
  main()
