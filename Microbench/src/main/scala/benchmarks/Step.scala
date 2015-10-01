package benchmarks

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Step {
  @Param(Array("2"))
  var step: Long = _

  var count: Long = 0

  def run(): Long = {
    count += 1
    count
  }
  def get(): Long = count
  def at(): Long = step
  def test(v: Long): Boolean = v % step == 0
}
