package rescala.meta.optimization

import rescala.meta.ReactiveGraph

trait MetaOptimization {
  val name: String
  val verbose: Boolean
  protected val protocol: String => Unit

  final def optimize(graph : ReactiveGraph): Unit = {
    if (verbose) protocol("Starting optimization " + name)
    if (verbose) protocol("Begin of analyze phase of optimization " + name)
    analyze(graph)
    if (verbose) protocol("End of analyze phase of optimization " + name)
    if (verbose) protocol("Begin of transform phase of optimization " + name)
    transform(graph)
    if (verbose) protocol("End of transform phase of optimization " + name)
  }
  protected def analyze(graph : ReactiveGraph): Unit
  protected def transform(graph : ReactiveGraph): Unit
}
