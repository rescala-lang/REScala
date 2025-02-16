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
  private val mixed: Regex         = """mixed ([\d_]+) ([\d_]+) ([\d_]+)""".r
  private val mp: Regex            = """mp ([\d_]+)""".r
  private val benchmark: Regex     = """benchmark""".r
  private val saveBenchmark: Regex = """save-benchmark""".r

  private def parseInt(str: String): Int = str.replace("_", "").toInt

  def startCLI(): Unit = {
    var running = true
    while running do {
      print("client> ")
      val line = Option(readLine()).map(_.strip())
      line match {
        case Some(commented())                 => // ignore
        case Some(get(key))                    => client.read(key)
        case Some(put(key, value))             => client.write(key, value)
        case Some(multiget(key, times))        => client.multiget(key, parseInt(times))
        case Some(multiput(key, value, times)) => client.multiput(key, value, parseInt(times))
        case Some(mp(times))                   => client.multiput("key%n", "value%n", parseInt(times))
        case Some(mixed(min, max, times))      => client.mixed(parseInt(min), parseInt(max), parseInt(times))
        case Some(benchmark()) =>
          client.doBenchmark = true
        case Some(saveBenchmark()) =>
          client.saveBenchmark(name)
        case None | Some("exit") => running = false
        case _ =>
          println("assuming put")
          client.write("key", "value")
      }
    }
    println(s"ended")
  }

}
