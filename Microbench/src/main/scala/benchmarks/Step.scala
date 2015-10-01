package benchmarks

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Step {
  @Param(Array("2"))
  var step: Long = _

  var count: Long = 0

  def isStep(): Boolean = {
    count += 1
    count % step == 0
  }
}
