package probench.clients

import probench.benchmark.{BenchmarkData, CSVWriter}
import rdts.base.Uid

import java.nio.file.Path
import scala.io.StdIn.readLine
import scala.util.matching.Regex

class ClientCLI(name: Uid, client: Client) {

  private val commented: Regex     = """#.*""".r
  private val get: Regex           = """get ([\w%]+)""".r
  private val put: Regex           = """put ([\w%]+) ([\w%]+)""".r
  private val multiget: Regex      = """multiget ([\w%]+) ([\d_]+)""".r
  private val multiput: Regex      = """multiput ([\w%]+) ([\w%]+) ([\d_]+)""".r
  private val mp: Regex            = """mp ([\d_]+)""".r
  private val benchmark: Regex     = """benchmark""".r
  private val saveBenchmark: Regex = """save-benchmark""".r

  def startCLI(): Unit = {
    var running = true
    while running do {
      print("client> ")
      val line = Option(readLine()).map(_.strip())
      line match {
        case Some(commented())                 => // ignore
        case Some(get(key))                    => client.read(key)
        case Some(put(key, value))             => client.write(key, value)
        case Some(multiget(key, times))        => client.multiget(key, times.replace("_", "").toInt)
        case Some(multiput(key, value, times)) => client.multiput(key, value, times.replace("_", "").toInt)
        case Some(mp(times))                   => client.multiput("key%n", "value%n", times.replace("_", "").toInt)
        case Some(benchmark()) =>
          client.doBenchmark = true
        case Some(saveBenchmark()) =>
          val env = System.getenv()

          val runId         = env.getOrDefault("RUN_ID", Uid.gen().delegate)
          val benchmarkPath = Path.of(env.getOrDefault("BENCH_RESULTS_DIR", "bench-results")).resolve(runId)
          val writer        = new CSVWriter(";", benchmarkPath, s"${name.delegate}-$runId", BenchmarkData.header)
          client.benchmarkData.foreach { row =>
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
        case None | Some("exit") => running = false
        case _ =>
          println("assuming put")
          client.write("key", "value")
      }
    }
    println(s"ended")
    System.exit(1)
  }

}
