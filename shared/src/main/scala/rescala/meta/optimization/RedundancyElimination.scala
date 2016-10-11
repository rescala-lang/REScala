package rescala.meta.optimization

import rescala.meta.{DataFlowGraph, DataFlowNode, ReactiveNode}

class RedundancyElimination(override val verbose: Boolean = false, override val protocol: String => Unit = println) extends MetaOptimization {
  override val name: String = "Redundancy elimination"
  private var redundantN1: Option[DataFlowNode[_]] = None
  private var redundantN2: Option[DataFlowNode[_]] = None

  override protected def analyze(graph: DataFlowGraph): Unit = {
    redundantN1 = None
    redundantN2 = None
    graph.nodes.collect{ case r:ReactiveNode[_] => r }.find {
      n =>
        redundantN1 = Some(n)
        redundantN2 = (graph.nodes - n).find(n2 => n2.structuralEquals(n) && !n2.hasReification)
        redundantN2.isDefined
    }
  }

  override protected def transform(graph: DataFlowGraph): Boolean = (redundantN1, redundantN2) match {
    case (Some(n1), Some(n2)) =>
      graph.nodeRefs(n2).foreach(graph.registerRef(_, n1))
      graph.deleteNode(n2)
      true
    case _ => false
  }
}

object RedundancyElimination extends RedundancyElimination(false, println)




