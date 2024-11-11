package probench

import probench.benchmark.{BenchmarkData, CSVWriter}
import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.contextual.CausalQueue
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

import java.nio.file.Path
import java.util.concurrent.Semaphore
import scala.collection.mutable
import scala.io.StdIn
import scala.io.StdIn.readLine
import scala.util.matching.Regex

class Client(val name: Uid) {

  given localUid: LocalUid = LocalUid(name)
  private val dataManager  = ProDataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onStateChange)

  private val lock                                             = new Object()
  private var currentOp: Option[Request]                       = None
  private var waitForOp: Boolean                               = true
  private var doBenchmark: Boolean                             = false
  private val benchmarkData: mutable.ListBuffer[BenchmarkData] = mutable.ListBuffer.empty

  private val commented: Regex     = """#.*""".r
  private val waitForRes: Regex    = """wait-for-res (true|false)""".r
  private val get: Regex           = """get ([\w%]+)""".r
  private val put: Regex           = """put ([\w%]+) ([\w%]+)""".r
  private val multiget: Regex      = """multiget ([\w%]+) ([\d_]+)""".r
  private val multiput: Regex      = """multiput ([\w%]+) ([\w%]+) ([\d_]+)""".r
  private val mp: Regex            = """mp ([\d_]+)""".r
  private val benchmark: Regex     = """benchmark""".r
  private val saveBenchmark: Regex = """save-benchmark""".r

  val requestSemaphore = new Semaphore(0)

  private def onStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    for {
      op                                                     <- currentOp
      CausalQueue.QueueElement(res @ Response(req, _), _, _) <- newState.responses.data.values if req == op
    } {
      println(res.payload)

      currentOp = None

      dataManager.transform(_.mod(state => state.copy(responses = state.responses.mod(_.removeBy(_ == res)))))

      requestSemaphore.release(1)
    }
  }

  private def handleOp(op: KVOperation[String, String]): Unit = {
    val req = Request(op)
    currentOp = Some(req)

    val start = if doBenchmark then System.nanoTime() else 0

    // TODO: still not sure that the semaphore use is correct …
    // its quite likely possible that some other request is answered after draining, causing the code below to return immediately
    // though overall currentOp is not protected at all, so it is triple unclear what is going on
    requestSemaphore.drainPermits()

    dataManager.transform { current =>
      current.mod(it => it.copy(requests = it.requests.mod(_.enqueue(req))))
    }

    if waitForOp then {
      requestSemaphore.acquire(1)

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
  }

  def read(key: String): Unit = {
    handleOp(KVOperation.Read(key))
  }

  def write(key: String, value: String): Unit = {
    handleOp(KVOperation.Write(key, value))
  }

  private def multiget(key: String, times: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do read(key.replace("%n", i.toString))
    println(s"Did $times get queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  private def multiput(key: String, value: String, times: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do write(key.replace("%n", i.toString), value.replace("%n", i.toString))
    println(s"Did $times put queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def startCLI(): Unit = {
    var running = true
    while running do {
      print("client> ")
      val line = Option(readLine()).map(_.strip())
      line match {
        case Some(commented())                 => // ignore
        case Some(get(key))                    => read(key)
        case Some(put(key, value))             => write(key, value)
        case Some(multiget(key, times))        => multiget(key, times.replace("_", "").toInt)
        case Some(multiput(key, value, times)) => multiput(key, value, times.replace("_", "").toInt)
        case Some(mp(times))                   => multiput("key%n", "value%n", times.replace("_", "").toInt)
        case Some(waitForRes(flag)) =>
          if doBenchmark then println("Can't change waiting mode while benchmarking!")
          else waitForOp = flag.toBoolean
        case Some(benchmark()) =>
          doBenchmark = true
          waitForOp = true
        case Some(saveBenchmark()) =>
          val env = System.getenv()

          val runId         = env.getOrDefault("RUN_ID", Uid.gen().delegate)
          val benchmarkPath = Path.of(env.getOrDefault("BENCH_RESULTS_DIR", "bench-results")).resolve(runId)
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
        case Some("ping")        => dataManager.pingAll()
        case Some("wait")        => lock.synchronized { lock.wait() }
        case None | Some("exit") => running = false
        case _ =>
          println("assuming put")
          write("key", "value")
      }
    }
    println(s"ended")
    System.exit(1)
  }

  export dataManager.addLatentConnection

}
