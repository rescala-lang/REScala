package probench.clients

import probench.benchmark.{BenchmarkData, CSVWriter}
import probench.data.KVOperation
import rdts.base.Uid

import java.nio.file.Path
import scala.collection.mutable

enum BenchmarkMode {
  case Read
  case Write
  case Mixed
}

trait Client(name: Uid) {

  var printResults                                     = true
  var doBenchmark: Boolean                             = false
  val benchmarkData: mutable.ListBuffer[BenchmarkData] = mutable.ListBuffer.empty

  def read(key: String): Unit                 = handleOp(KVOperation.Read(key))
  def write(key: String, value: String): Unit = handleOp(KVOperation.Write(key, value))

  def multiget(key: String, times: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do read(key.replace("%n", i.toString))
    println(s"Did $times get queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def multiput(key: String, value: String, times: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do write(key.replace("%n", i.toString), value.replace("%n", i.toString))
    println(s"Did $times put queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def mixed(min: Int, max: Int, times: Int = 1): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do {
      val num = Math.round(Math.random() * (max - min) + min).toInt
      if Math.random() > 0.5 then
        read(f"key$num")
      else
        write(f"key$num", f"value$num")
    }
    println(s"Did $times mixed queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def benchmark(mode: BenchmarkMode, times: Int, min: Int = 1, max: Int = 10000): Unit = {
    printResults = false

    println("Initializing")

    mode match
      case BenchmarkMode.Read | BenchmarkMode.Mixed => multiput("key%n", "value", max)
      case BenchmarkMode.Write                      =>

    println("Warmup")

    val warmupStart = System.currentTimeMillis()

    mode match
      case BenchmarkMode.Read  => multiget("key%n", 100000)
      case BenchmarkMode.Write => multiput("warmup%n", "value%n", 100000)
      case BenchmarkMode.Mixed => mixed(1, 1000, 100000)

    println("Measurement")

    doBenchmark = true

    mode match
      case BenchmarkMode.Read  => multiget("key%n", times)
      case BenchmarkMode.Write => multiput("key%n", "value%n", times)
      case BenchmarkMode.Mixed => mixed(min, max, times)

    saveBenchmark(name)
  }

  def benchmarkTimed(warmup: Int, measurement: Int, mode: BenchmarkMode): Unit = {
    printResults = false

    println("Initializing")

    mode match
      case BenchmarkMode.Read | BenchmarkMode.Mixed => multiput(s"${name.delegate}-key%n", "value%n", 1000)
      case BenchmarkMode.Write                      =>

    println("Warmup")

    val warmupStart = System.currentTimeMillis()

    while (System.currentTimeMillis() - warmupStart) < warmup * 1000 do {
      mode match
        case BenchmarkMode.Read  => multiget(s"${name.delegate}-key", 1000)
        case BenchmarkMode.Write => multiput(s"${name.delegate}-key", "value", 1000)
        case BenchmarkMode.Mixed => mixed(1, 1000, 1000)
    }

    println("Measurement")

    val measurementStart = System.currentTimeMillis()
    doBenchmark = true

    while (System.currentTimeMillis() - measurementStart) < measurement * 1000 do {
      mode match
        case BenchmarkMode.Read  => multiget(s"${name.delegate}-key", 1000)
        case BenchmarkMode.Write => multiput(s"${name.delegate}-key", "value", 1000)
        case BenchmarkMode.Mixed => mixed(1, 1000, 1000)
    }

    saveBenchmark(name)
  }

  def handleOp(op: KVOperation[String, String]): Unit = {
    val start = if doBenchmark then System.nanoTime() else 0

    handleOpImpl(op)

    if doBenchmark then {
      val end = System.nanoTime()
      val opString = op match
        case KVOperation.Read(_)     => "get"
        case KVOperation.Write(_, _) => "put"
      val args = op match
        case KVOperation.Read(key)         => key
        case KVOperation.Write(key, value) => s"$key $value"
      benchmarkData.append(BenchmarkData(
        name.delegate,
        opString,
        args,
        start / 1000,
        end / 1000,
        (end - start).toDouble / 1000,
        "µs"
      ))
    }
  }

  def saveBenchmark(name: Uid): Unit = {
    println("Saving Benchmark Data")
    val env           = System.getenv()
    val runId         = env.getOrDefault("RUN_ID", Uid.gen().delegate)
    val system = env.getOrDefault("SYSTEM_ID", "pb")
    val benchmarkPath = Path.of(env.getOrDefault("BENCH_RESULTS_DIR", "bench-results")).resolve(system).resolve(runId)
    val writer        = new CSVWriter(";", benchmarkPath, s"${name.delegate}-$runId", BenchmarkData.header)
    benchmarkData.foreach { row =>
      writer.writeRow(
        s"${row.name}",
        row.op,
        row.args,
        row.sendTime.toString,
        row.receiveTime.toString,
        row.latency.toString,
        row.unit
      )
    }
    writer.close()
  }

  def handleOpImpl(op: KVOperation[String, String]): Unit

  def onResultValue(result: String): Unit = {
    if printResults then println(result)
  }

}
