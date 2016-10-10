package rescala.meta.optimization

import rescala.meta.{DataFlowGraph, MappedEventNode, MappedSignalNode}

class MapFusion(override val verbose: Boolean = false, override val protocol: String => Unit = println) extends MetaOptimization {
  override val name: String = "Operator fusion"

  override protected def analyze(graph: DataFlowGraph): Unit = if (verbose) {
    var countE = 0
    var countS = 0
    graph.nodes.foreach {
      case nOuter@MappedEventNode(_, outerBase, outerMap) =>
        outerBase.node match {
          case Some(nInner@MappedEventNode(_, innerBase, innerMap)) =>
            if ((graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              countE += 1
            }
          case _ => ()
        }
      case nOuter@MappedSignalNode(_, outerBase, outerMap) =>
        outerBase.node match {
          case Some(nInner@MappedSignalNode(_, innerBase, innerMap)) =>
            if ((graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              countS += 1
            }
          case _ => ()
        }
      case _ => ()
    }
    protocol("Fusing " + countE + " mapped events and " + countS + " mapped signals.")
  }

  override protected def transform(graph: DataFlowGraph): Boolean = {
    var transformed = false
    graph.nodes.foreach {
      case nOuter@MappedEventNode(_, outerBase, outerMap) =>
        outerBase.node match {
          case Some(nInner@MappedEventNode(_, innerBase, innerMap)) =>
            if ((graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              val newNode = MappedEventNode(graph, innerBase, innerMap.asInstanceOf[Function[Any, Any]].andThen(outerMap.asInstanceOf[Function[Any, Any]]))
              graph.pointersForNode(nInner).foreach(graph.deletePointer)
              graph.pointersForNode(nOuter).foreach(graph.registerPointer(_, newNode))
              graph.deleteNode(nInner)
              graph.deleteNode(nOuter)
              transformed = true
            }
          case _ => ()
        }
      case nOuter@MappedSignalNode(_, outerBase, outerMap) =>
        outerBase.node match {
          case Some(nInner@MappedSignalNode(_, innerBase, innerMap)) =>
            if ((graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              val newNode = MappedSignalNode(graph, innerBase, innerMap.asInstanceOf[Function[Any, Any]].andThen(outerMap.asInstanceOf[Function[Any, Any]]))
              graph.pointersForNode(nInner).foreach(graph.deletePointer)
              graph.pointersForNode(nOuter).foreach(graph.registerPointer(_, newNode))
              graph.deleteNode(nInner)
              graph.deleteNode(nOuter)
              transformed = true
            }
          case _ => ()
        }
      case _ => ()
    }
    transformed
  }
}

object MapFusion extends MapFusion(false, println)


