package reactives.meta.optimization

import reactives.meta._

case class NodeTriple(
    nOuter: DataFlowNode[_],
    nInner: DataFlowNode[_],
    newNode: DataFlowNode[_]
)

class OperatorFusion(override val verbose: Boolean = false, override val protocol: String => Unit = println)
    extends MetaOptimization[NodeTriple] {
  override val name: String = "Operator fusion"

  override protected def analyze(graph: DataFlowGraph): Option[NodeTriple] = {
    def inner(nodes: List[DataFlowNode[_]]): Option[NodeTriple] = {
      nodes match {
        case (nOuter @ MappedEventNode(_, outerBase, outerMap)) :: tail =>
          outerBase.tryDeref match {
            case Some(nInner @ MappedEventNode(_, innerBase, innerMap))
                if !nOuter.hasReification && !nInner.hasReification && (graph.outgoingDataFlow(
                  nInner
                ) - nOuter).isEmpty =>
              val newNode = MappedEventNode(
                graph,
                innerBase,
                innerMap.asInstanceOf[Function[Any, Any]].andThen(outerMap.asInstanceOf[Function[Any, Any]])
              )
              Some(NodeTriple(nOuter, nInner, newNode))
            case _ => inner(tail)
          }
        case (nOuter @ FilteredEventNode(_, outerBase, outerFilter)) :: tail =>
          outerBase.tryDeref match {
            case Some(nInner @ FilteredEventNode(_, innerBase, innerFilter))
                if !nOuter.hasReification && !nInner.hasReification && (graph.outgoingDataFlow(
                  nInner
                ) - nOuter).isEmpty =>
              val newNode = FilteredEventNode(
                graph,
                innerBase, {
                  x: Any =>
                    innerFilter.asInstanceOf[Function[Any, Boolean]](x) && outerFilter.asInstanceOf[Function[
                      Any,
                      Boolean
                    ]](x)
                }
              )
              Some(NodeTriple(nOuter, nInner, newNode))
            case _ => inner(tail)
          }
        case (nOuter @ MappedSignalNode(_, outerBase, outerMap)) :: tail =>
          outerBase.tryDeref match {
            case Some(nInner @ MappedSignalNode(_, innerBase, innerMap))
                if !nOuter.hasReification && !nInner.hasReification && (graph.outgoingDataFlow(
                  nInner
                ) - nOuter).isEmpty =>
              val newNode = MappedSignalNode(
                graph,
                innerBase,
                innerMap.asInstanceOf[Function[Any, Any]].andThen(outerMap.asInstanceOf[Function[Any, Any]])
              )
              Some(NodeTriple(nOuter, nInner, newNode))
            case _ => inner(tail)
          }
        case _ :: tail => inner(tail)
        case Nil       => None

      }
    }

    inner(graph.nodes.toList)
  }

  override protected def transform(graph: DataFlowGraph, nodeTriple: NodeTriple): Boolean = {
    val NodeTriple(nOut, nIn, newN) = nodeTriple
    graph.nodeRefs(nIn).foreach(graph.deleteRef)
    graph.nodeRefs(nOut).foreach(graph.registerRef(_, newN))
    graph.deleteNode(nIn)
    graph.deleteNode(nOut)
    true
  }
}

object OperatorFusion extends OperatorFusion(false, println)
