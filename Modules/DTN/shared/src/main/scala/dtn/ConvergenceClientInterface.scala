package dtn

import rdts.time.Dots

object NoDotsConvergenceClient extends ConvergenceClientInterface {
  override def send(dots: Dots): Unit = ()
}

trait ConvergenceClientInterface {
  def send(dots: Dots): Unit
}
