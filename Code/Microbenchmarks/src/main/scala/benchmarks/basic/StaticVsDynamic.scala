package benchmarks.basic

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReadWriteLock

import benchmarks.{EngineParam, Step, Workload}
import org.openjdk.jmh.annotations.{
  Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Param, Scope, Setup, State, Threads, Warmup
}
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.core.{Scheduler, Struct}
import rescala.interface.RescalaInterface
import rescala.reactives.{Signal}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class StaticVsDynamic[S <: Struct] {

  var engine: RescalaInterface[S]      = _
  implicit def scheduler: Scheduler[S] = engine.scheduler

  val engineT = engine
  import engineT.Var

  @Param(Array("true", "false"))
  var static: Boolean = _

  var source: Var[Boolean] = _
  var current: Boolean     = _
  var lock: ReadWriteLock  = _
  var a: Var[Int]          = _
  var b: Var[Int]          = _
  var res: Signal[Int, S]  = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]): Unit = {
    engine = engineParam.engine
    current = true
    source = engineT.Var(current)
    a = engineT.Var { 10 }
    b = engineT.Var { 20 }

    val e = engine
    import e._
    if (static) engine.Signals.static(source, a, b) { st =>
      if (st.dependStatic(source)) st.dependStatic(a) else st.dependStatic(b)
    }
    else engine.Signal.dynamic { if (source()) a() else b() }

  }

  @Benchmark
  def switchOnly(): Unit = {
    current = !current
    source.set(current)
  }

  @Benchmark
  def aOnly(step: Step): Unit = {
    a.set(step.run())
  }
  @Benchmark
  def bOnly(step: Step): Unit = {
    b.set(step.run())
  }
}
