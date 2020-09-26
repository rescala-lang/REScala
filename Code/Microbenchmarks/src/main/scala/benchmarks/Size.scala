package benchmarks

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Size {
  @Param(Array("2"))
  var size: Int = _
}
