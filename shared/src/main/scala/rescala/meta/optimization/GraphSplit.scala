package rescala.meta.optimization

import rescala.meta.{ReactiveGraph, ReactiveNode}

class GraphSplit(override val verbose: Boolean = false, override val protocol: String => Unit = println) extends MetaOptimization {
  override val name: String = "Graph splitter"
  private var nodeGroups: Set[Set[ReactiveNode[_]]] = Set()
  var splittedGraphs : Set[ReactiveGraph] = Set()

  override protected def analyze(graph: ReactiveGraph): Unit = {
    nodeGroups = graph.nodes.map(n => Set[ReactiveNode[_]](n))

    var changed = true
    while (changed) {
      changed = false
      nodeGroups = nodeGroups.foldLeft(Set[Set[ReactiveNode[_]]]())((groups, next) => {
        groups.find(_.exists(n => n.dependencies.exists(next.contains) || next.exists(_.dependencies.contains(n)))) match {
          case Some(g) =>
            changed = true
            (groups - g) + (g ++ next)
          case None => groups + next
        }
      })
    }
    if (verbose) protocol("Found " + nodeGroups.size + " groups of distinct nodes.")
  }

  override protected def transform(graph: ReactiveGraph): Unit = {
    splittedGraphs = Set()
     for (group <- nodeGroups) {
       val newGraph = new ReactiveGraph()
       graph.moveNodes(group, newGraph)
       splittedGraphs = splittedGraphs + newGraph
     }
  }
}

object GraphSplit extends GraphSplit(false, println)

