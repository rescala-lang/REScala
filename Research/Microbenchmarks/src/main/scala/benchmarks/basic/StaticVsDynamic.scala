package benchmarks.basic

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReadWriteLock

import benchmarks.{EngineParam, Step, Workload}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Param, Scope, Setup, State, Threads, Warmup}
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.core.{Scheduler, Struct}
import rescala.reactives.{Signal, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class StaticVsDynamic[S <: Struct] {

  implicit var engine: Scheduler[S] = _

  @Param(Array("true", "false"))
  var static: Boolean = _

  var source: Var[Boolean, S] = _
  var current: Boolean = _
  var lock: ReadWriteLock = _
  var a: Var[Int, S] = _
  var b: Var[Int, S] = _
  var res: Signal[Int, S] = _


  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]): Unit = {
    engine = engineParam.engine
    current = true
    source = engine.Var(current)
    a = engine.Var { 10 }
    b = engine.Var { 20 }

    if (static) engine.Signals.static(source, a, b){st =>
      if (st.dependStatic(source)) st.dependStatic(a) else st.dependStatic(b)}
    else engine.Signal.dynamic { if (source()) a() else b() }

  }

  @Benchmark
  def switchOnly(): Unit = {
      current = !current
      source.set(current)
  }

  @Benchmark
  def aOnly(step:Step): Unit = {
    a.set(step.run())
  }
  @Benchmark
  def bOnly(step:Step): Unit = {
    b.set(step.run())
  }
}
