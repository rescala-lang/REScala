package benchmarks.basic

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.Schedulers
import rescala.interface.RescalaInterface

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Benchmark)
class SingleVar {

  var engine: RescalaInterface              = _
  lazy val engineT                          = engine
  implicit def scheduler: engineT.Scheduler = engineT.scheduler

  var source: engineT.Var[Boolean] = _
  var current: Boolean             = _
  var lock: ReadWriteLock          = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    current = false
    source = engineT.Var(current)
    if (engineParam.engine.scheduler == Schedulers.unmanaged) lock = new ReentrantReadWriteLock()
  }

  @Benchmark
  def write(): Unit = {
    if (lock == null) {
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
    if (lock == null) {
      source.readValueOnce
    } else {
      lock.readLock().lock()
      try {
        source.readValueOnce
      } finally lock.readLock().unlock()
    }
  }

}
