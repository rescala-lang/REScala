package benchmarks.crypto

import lofi_acl.access.KeyDerivationKey
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class KeyDerivationKeyBench {
  val ikm = Array[Byte](
    99, -30, -25, -90, -76, 97, -33, 101, -109, -99, 17, 3, 73, -126, 51, 39, -107, 26, 57, 45, 104, -54, -15, -59, -2,
    84, -116, 107, 0, 18, 52, 22
  )
  val kdk                 = KeyDerivationKey(ikm)
  var path: Array[String] = scala.compiletime.uninitialized

  @Param(Array("1", "10", "100"))
  var pathLength: Int = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    path = (0 until pathLength).toArray map { idx => s"path-element-$idx" }
  }

  @Benchmark
  def derivePath(): Unit = {
    val _ = kdk.recursiveChildKeyDerivationKey(path)
  }
}
