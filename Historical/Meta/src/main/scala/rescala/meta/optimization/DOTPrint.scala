package reactives.meta.optimization
import reactives.meta.DataFlowGraph

class DOTPrint(
    val graphName: String = "graph",
    override val verbose: Boolean = false,
    override val protocol: String => Unit = println
) extends MetaOptimization[Unit] {
  override val name: String = "DOT graph printer"

  override protected def analyze(graph: DataFlowGraph): Option[Unit] = {
    val builder          = StringBuilder.newBuilder
    val elementsNumbered = graph.nodes.zip(1 to graph.nodes.size)
    builder ++= "digraph " + graphName + "{\n"
    for ((elem, num) <- elementsNumbered) {
      builder ++= "r" + num + "[label=\"" + elem.toString + "\"];\n"
    }
    for (
      (elem, num) <- elementsNumbered; (out, outNum) <- elem.dependencies.map(d => elementsNumbered.find(_._1 == d).get)
    ) {
      builder ++= "r" + num + " -> r" + outNum + ";\n"
    }
    builder ++= "\n}"
    protocol(builder.toString())
    None
  }

  override protected def transform(graph: DataFlowGraph, param: Unit): Boolean = false
}

object DOTPrint extends DOTPrint("graph", false, println)
