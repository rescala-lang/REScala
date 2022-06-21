package benchmarks.encrdt

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.results.format.ResultFormatType
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder

import java.nio.file.Paths
import java.util
import java.util.concurrent.{Callable, Executors, TimeUnit}

object BenchmarkRunnerApp extends App {
  Paths.get("benchmarks/results/").toFile.mkdirs()

  val timeBefore = System.currentTimeMillis()

  val jmhOptions = new OptionsBuilder()
    .include("serializeOnly|encryptOnly")
    .resultFormat(ResultFormatType.CSV)
    .result("benchmarks/results/jmh_benchmark.csv")
    .forks(3)
    .build()

  val results: util.Collection[RunResult] = new Runner(jmhOptions).run()

  println("Running ToDo App Benchmarks")
  ToDoAppBenchmark.main(Array.empty)

  println("Running size benchmarks")
  private val sizeBenchmarks = List[Callable[Unit]](
    () => DeltaStateBasedUntrustedReplicaSizeBenchmark.main(Array.empty),
    () => DeltaStateBasedUntrustedReplicaSizeBenchmarkLinearScaling.main(Array.empty),
    () => StateBasedUntrustedReplicaSizeBenchmark.main(Array.empty),
  )

  private val executorService = Executors.newFixedThreadPool(4)
  sizeBenchmarks.foreach((task: Callable[Unit]) => executorService.submit(task))
  executorService.shutdown()
  executorService.awaitTermination(1, TimeUnit.HOURS)

  val timeAfter = System.currentTimeMillis()
  println("Finished running size benchmarks (in " + (timeAfter - timeBefore) + "ms)")
}
