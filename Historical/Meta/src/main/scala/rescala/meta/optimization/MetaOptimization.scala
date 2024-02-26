package reactives.meta.optimization

import reactives.meta.DataFlowGraph

trait MetaOptimization[T] {
  val name: String
  val verbose: Boolean
  protected val protocol: String => Unit

  final def optimize(graph: DataFlowGraph): Unit = {
    if (verbose) protocol("Starting optimization " + name)
    var iterate = true
    while (iterate) {
      if (verbose) protocol("Begin of analyze phase of optimization " + name)
      val param = analyze(graph)
      if (verbose) protocol("End of analyze phase of optimization " + name)
      if (verbose) protocol("Begin of transform phase of optimization " + name)
      iterate = param.exists(transform(graph, _))
      if (verbose) protocol("End of transform phase of optimization " + name)
      if (iterate && verbose) protocol("Re-iterating optimization " + name)
    }
    if (verbose) protocol("Ending optimization " + name)
  }
  protected def analyze(graph: DataFlowGraph): Option[T]
  protected def transform(graph: DataFlowGraph, param: T): Boolean
}
