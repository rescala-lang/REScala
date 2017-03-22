package examples.demo.system

import rescala._

object Clock {
  private val _time = Var(System.nanoTime())
  def tick() = _time.set(System.nanoTime())

  val nsTime: Signal[Long] = _time

  val NanoSecond = 1e9d
}
