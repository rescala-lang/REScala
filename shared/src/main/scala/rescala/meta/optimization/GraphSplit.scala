package rescala.meta.optimization

import rescala.meta.{DataFlowGraph, DataFlowNode}

class GraphSplit(override val verbose: Boolean = false, override val protocol: String => Unit = println) extends MetaOptimization {
  override val name: String = "Graph splitter"
  private var nodeGroups: Set[Set[DataFlowNode[_]]] = Set()
  var splittedGraphs : Set[DataFlowGraph] = Set()

  override protected def analyze(graph: DataFlowGraph): Unit = {
    nodeGroups = graph.nodes.map(n => Set[DataFlowNode[_]](n))

    var changed = true
    while (changed) {
      changed = false
      nodeGroups = nodeGroups.foldLeft(Set[Set[DataFlowNode[_]]]())((groups, next) => {
        groups.find(_.exists(n => graph.incomingDependencies(n).exists(next.contains) || next.exists(graph.incomingDependencies(_).contains(n)))) match {
          case Some(g) =>
            changed = true
            (groups - g) + (g ++ next)
          case None => groups + next
        }
      })
    }
    if (verbose) protocol("Found " + nodeGroups.size + " groups of distinct nodes.")
  }

  override protected def transform(graph: DataFlowGraph): Boolean = {
    splittedGraphs = Set()
     for (group <- nodeGroups) {
       val newGraph = new DataFlowGraph()
       graph.moveNodes(group, newGraph)
       splittedGraphs = splittedGraphs + newGraph
     }
    false
  }
}

object GraphSplit extends GraphSplit(false, println)

