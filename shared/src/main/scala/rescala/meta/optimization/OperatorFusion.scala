package rescala.meta.optimization

import rescala.meta._

class OperatorFusion(override val verbose: Boolean = false, override val protocol: String => Unit = println) extends MetaOptimization {
  override val name: String = "Operator fusion"
  private var nOuter: Option[DataFlowNode[_]] = None
  private var nInner: Option[DataFlowNode[_]] = None
  private var newNode: Option[DataFlowNode[_]] = None

  override protected def analyze(graph: DataFlowGraph): Unit = {
    nOuter = None
    nInner = None
    newNode = None
    graph.nodes.find {
      case nOuter@MappedEventNode(_, outerBase, outerMap) =>
        outerBase.deref match {
          case Some(nInner@MappedEventNode(_, innerBase, innerMap)) =>
            if (!nOuter.hasReification && !nInner.hasReification && (graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              val newNode = MappedEventNode(graph, innerBase, innerMap.asInstanceOf[Function[Any, Any]].andThen(outerMap.asInstanceOf[Function[Any, Any]]))
              this.nOuter = Some(nOuter)
              this.nInner = Some(nInner)
              this.newNode = Some(newNode)
              true
            } else false
          case _ => false
        }
      case nOuter@FilteredEventNode(_, outerBase, outerFilter) =>
        outerBase.deref match {
          case Some(nInner@FilteredEventNode(_, innerBase, innerFilter)) =>
            if (!nOuter.hasReification && !nInner.hasReification && (graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              val newNode = FilteredEventNode(graph, innerBase, { x: Any => innerFilter.asInstanceOf[Function[Any, Boolean]](x) && outerFilter.asInstanceOf[Function[Any, Boolean]](x) })
              this.nOuter = Some(nOuter)
              this.nInner = Some(nInner)
              this.newNode = Some(newNode)
              true
            } else false
          case _ => false
        }
      case nOuter@MappedSignalNode(_, outerBase, outerMap) =>
        outerBase.deref match {
          case Some(nInner@MappedSignalNode(_, innerBase, innerMap)) =>
            if (!nOuter.hasReification && !nInner.hasReification && (graph.outgoingDependencies(nInner) - nOuter).isEmpty) {
              val newNode = MappedSignalNode(graph, innerBase, innerMap.asInstanceOf[Function[Any, Any]].andThen(outerMap.asInstanceOf[Function[Any, Any]]))
              this.nOuter = Some(nOuter)
              this.nInner = Some(nInner)
              this.newNode = Some(newNode)
              true
            } else false
          case _ => false
        }
      case _ => false
    }
  }

  override protected def transform(graph: DataFlowGraph): Boolean = (nOuter, nInner, newNode) match {
    case (Some(nOut), Some(nIn), Some(newN)) =>
      graph.nodeRefs(nIn).foreach(graph.deleteRef)
      graph.nodeRefs(nOut).foreach(graph.registerRef(_, newN))
      graph.deleteNode(nIn)
      graph.deleteNode(nOut)
      true
    case _ => false
  }
}

object OperatorFusion extends OperatorFusion(false, println)


