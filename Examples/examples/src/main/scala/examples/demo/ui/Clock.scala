package examples.demo.ui

import rescala._

object Clock {
  private val _time = Var(System.nanoTime())
  val time: Signal[Long] = _time

  def tick() = _time.set(System.nanoTime())

  val NanoSecond = 1e9d
}
