package benchmarks.crypto

import org.openjdk.jmh.annotations.*

import java.security.SecureRandom
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class SecureRandomBench {

  var secureRandomStrong: SecureRandom = scala.compiletime.uninitialized
  var secureRandomNormal: SecureRandom = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    this.secureRandomStrong = SecureRandom.getInstanceStrong
    this.secureRandomNormal = SecureRandom()
  }

  @Benchmark
  def generate256BytesStrong(): Unit = {
    secureRandomStrong.nextBytes(Array.ofDim[Byte](32))
  }

  @Benchmark
  def generate256BytesNormal(): Unit = {
    secureRandomNormal.nextBytes(Array.ofDim[Byte](32))
  }
}
