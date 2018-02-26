package benchmarks.basic

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.Engines
import rescala.core.{Scheduler, Struct}
import rescala.reactives.Var

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Benchmark)
class SingleVar[S <: Struct] {

  implicit var engine: Scheduler[S] = _

  var source: Var[Boolean, S] = _
  var current: Boolean = _
  var lock: ReadWriteLock = _


  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]): Unit = {
    engine = engineParam.engine
    current = false
    source = engine.Var(current)
    if (engineParam.engine == Engines.unmanaged) lock = new ReentrantReadWriteLock()
  }

  @Benchmark
  def write(): Unit = {
    if (lock == null) {
      current = !current
      source.set(current)
    }
    else {
      lock.writeLock().lock()
      try {
        current = !current
        source.set(current)
      }
      finally lock.writeLock().unlock()
    }
  }

  @Benchmark
  def read(): Boolean = {
    if (lock == null) {
      source.readValueOnce
    }
    else {
      lock.readLock().lock()
      try {
        source.readValueOnce
      }
      finally lock.readLock().unlock()
    }
  }

}
