package benchmarks.basic

import benchmarks.EngineParam
import org.openjdk.jmh.annotations.*
import reactives.SelectedScheduler.State as BundleState
import reactives.core.Scheduler

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Benchmark)
class SingleVar {

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val engineT             = engine

  var source: engineT.Var[Boolean] = scala.compiletime.uninitialized
  var current: Boolean             = scala.compiletime.uninitialized
  var lock: ReadWriteLock          = scala.compiletime.uninitialized

  @Setup
  def setup(engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    current = false
    source = engineT.Var(current)
    if reactives.SelectedScheduler.candidate.scheduler == reactives.scheduler.LevelbasedVariants.unmanaged then
      lock = new ReentrantReadWriteLock()
  }

  @Benchmark
  def write(): Unit = {
    if lock eq null then {
      current = !current
      source.set(current)
    } else {
      lock.writeLock().lock()
      try {
        current = !current
        source.set(current)
      } finally lock.writeLock().unlock()
    }
  }

  @Benchmark
  def read(): Boolean = {
    if lock eq null then {
      source.readValueOnce
    } else {
      lock.readLock().lock()
      try {
        source.readValueOnce
      } finally lock.readLock().unlock()
    }
  }

}
