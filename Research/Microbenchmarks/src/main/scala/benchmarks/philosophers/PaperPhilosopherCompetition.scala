package benchmarks.philosophers

import java.util.concurrent.TimeUnit

import benchmarks.{BusyThreads, EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.core.Struct

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
//@Fork(value=1, jvmArgsPrepend = Array("-XX:+PrintCompilation", "-XX:+PrintGCDetails"))
@Fork(1)
@Threads(2)
class PaperPhilosopherCompetition[S <: Struct] {
  @Benchmark
  def eatOnce(comp: PaperCompetition[S], params: ThreadParams, work: Workload): Unit = {
    if(comp.philosophers > 0) {
      comp.table.eatRandomOnce(params.getThreadIndex, params.getThreadCount)
    } else {
      comp.table.eatOnce(params.getThreadIndex * -comp.philosophers)
    }
  }
}

@State(Scope.Benchmark)
class PaperCompetition[S <: Struct] extends BusyThreads {
  @Param(Array("dynamic","semi-static", "static"))
  var dynamicity: String = _
  /**
    * philosophers > 0 mean that the table has this many philosophers, and they are distributed to threads round robin, each thread picks one assigned philosophers for each iteration.
    * philosophers = 0 means that all threads update the same philosopher on a table with 3 seats
    * philosophers < 0 mean that threads are placed with every (-philosophers) place with a table precisely as large as needed (except no less than 3 philosophers).
    * philosophers < -4 should be pointless (on -4 there are no more interactions, less than that just increases dead space between active philosophers)
    */
  @Param(Array("-4", "-3", "-2", "-1", "0", "16", "32", "64", "128"))
  var philosophers: Int = _
  @Param(Array("event", "signal", "none", "singleFold"))
  var topper: String = _
  var table: PaperPhilosophers[S] = _

  @Setup(Level.Trial)
  def printSystemStats() = {
    var assertions = false
    @inline def captureAssertionsEnabled = {
      assertions = true
      true
    }
    assert(captureAssertionsEnabled)
    println("Running on " + Runtime.getRuntime.availableProcessors() + " cores with assertions " + (if(assertions) "enabled." else "disabled."))
  }

//  var stream: PrintStream = _
  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]) = {
    val dynamic = dynamicity match {
      case "dynamic" => Dynamicity.Dynamic
      case "semi-static" => Dynamicity.SemiStatic
      case "static" => Dynamicity.Static
      case otherwise => throw new IllegalArgumentException("not a valid dynamicity: " + otherwise)
    }
    val size = if(philosophers > 0) philosophers else Math.max(3, -philosophers * params.getThreads)
    table = if(engineParam.engineName == "unmanaged") {
      topper match {
        case "event" => new PaperPhilosophers(size, engineParam.engine, dynamic) with EventPyramidTopper[S] with ManualLocking[S]
        case "signal" => new PaperPhilosophers(size, engineParam.engine, dynamic) with SignalPyramidTopper[S] with ManualLocking[S]
        case "singleFold" => new PaperPhilosophers(size, engineParam.engine, dynamic) with SingleFoldTopper[S] with ManualLocking[S]
        case "none" => new PaperPhilosophers(size, engineParam.engine, dynamic) with NoTopper[S] with ManualLocking[S]
        case otherwise => throw new IllegalArgumentException("not a valid topper: " + otherwise)
      }
    } else {
      topper match {
        case "event" => new PaperPhilosophers(size, engineParam.engine, dynamic) with EventPyramidTopper[S]
        case "signal" => new PaperPhilosophers(size, engineParam.engine, dynamic) with SignalPyramidTopper[S]
        case "singleFold" => new PaperPhilosophers(size, engineParam.engine, dynamic) with SingleFoldTopper[S]
        case "none" => new PaperPhilosophers(size, engineParam.engine, dynamic) with NoTopper[S]
        case otherwise => throw new IllegalArgumentException("not a valid topper: " + otherwise)
      }
    }

//    if(engineParam.engineName == "fullmv") {
//      SerializationGraphTracking.clear()
//      stream = new PrintStream(new FileOutputStream(s"fullmv-sgt-contention-$philosophers-${params.getThreads}-${System.currentTimeMillis()}.txt"))
//    }
  }

//  @TearDown(Level.Iteration)
//  def writeLockContentionTimer(): Unit = {
//      if(stream != null) {
//        SerializationGraphTracking.printStatistics(stream)
//        stream.close()
//    }
//  }
}
