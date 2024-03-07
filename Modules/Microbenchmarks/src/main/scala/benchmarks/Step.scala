package benchmarks

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Step {
  @Param(Array("2"))
  var step: Int = scala.compiletime.uninitialized

  var count: Int = 0

  def run(): Int = {
    count += 1
    count
  }
  def get(): Int            = count
  def at(): Int             = step
  def test(v: Int): Boolean = v % step == 0
}
