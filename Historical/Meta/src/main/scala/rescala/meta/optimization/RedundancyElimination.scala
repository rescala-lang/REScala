package reactives.meta.optimization

import reactives.meta.{DataFlowGraph, DataFlowNode, ReactiveNode}

case class RedundantNodes(original: DataFlowNode[_], redundant: DataFlowNode[_])

class RedundancyElimination(override val verbose: Boolean = false, override val protocol: String => Unit = println)
    extends MetaOptimization[RedundantNodes] {
  override val name: String = "Redundancy elimination"

  override protected def analyze(graph: DataFlowGraph): Option[RedundantNodes] = {
    @scala.annotation.tailrec
    def inner(nodes: List[ReactiveNode[_]]): Option[RedundantNodes] =
      nodes match {
        case n :: tail =>
          val r = (graph.nodes - n).find(n2 => n2.structuralEquals(n))
          r match {
            case None      => inner(tail)
            case Some(red) => Some(RedundantNodes(n, red))
          }
        case Nil => None
      }

    inner(graph.nodes.collect { case r: ReactiveNode[_] => r }.toList)
  }

  override protected def transform(graph: DataFlowGraph, param: RedundantNodes): Boolean = {
    val RedundantNodes((n1), (n2)) = param
    graph.nodeRefs(n2).foreach(graph.registerRef(_, n1))
    graph.deleteNode(n2)
    true
  }
}

object RedundancyElimination extends RedundancyElimination(false, println)
