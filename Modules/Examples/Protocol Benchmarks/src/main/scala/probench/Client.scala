package probench

import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.contextual.CausalQueue
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

import scala.io.StdIn
import scala.io.StdIn.readLine
import scala.util.matching.Regex

class Client(val name: Uid) {

  given localUid: LocalUid = LocalUid(name)
  private val dataManager  = DataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onStateChange)

  private val lock                       = new Object()
  private var currentOp: Option[Request] = None
  private var waitForOp: Boolean         = true

  private val commented: Regex  = """#.*""".r
  private val waitForRes: Regex = """wait-for-res (true|false)""".r
  private val get: Regex        = """get ([\w%]+)""".r
  private val put: Regex        = """put ([\w%]+) ([\w%]+)""".r
  private val multiget: Regex   = """multiget ([\w%]+) (\d+)""".r
  private val multiput: Regex   = """multiput ([\w%]+) ([\w%]+) (\d+)""".r

  private def onStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    /* val diff = newState.responses.data.values.size - oldState.responses.data.values.size
    if diff > 0 then {
      println(s"Got $diff result(s): ${newState.responses.data.values.toList.reverseIterator.take(diff).toList.reverse.map(_.value)}")
    } */

    for {
      op                                                     <- currentOp
      CausalQueue.QueueElement(res @ Response(req, _), _, _) <- newState.responses.data.values if req == op
    } {
      println(res.payload)

      currentOp = None

      dataManager.transform(_.mod(state => state.copy(responses = state.responses.mod(_.removeBy(_ == res)))))

      lock.synchronized {
        lock.notifyAll()
      }
    }
  }

  private def handleOp(op: KVOperation[String, String]): Unit = {
    val req = Request(op)
    currentOp = Some(req)

    // println(s"Put $req")

    dataManager.transform { current =>
      current.mod(it => it.copy(requests = it.requests.mod(_.enqueue(req))))
    }

    // println(s"New Requests ${dataManager.mergedState.data.requests.data.values.toList.map(_.value)}")

    if waitForOp then {
      lock.synchronized {
        lock.wait()
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
    for i <- 1 to times do read(key.replace("%n", i.toString))
  }

  private def multiput(key: String, value: String, times: Int): Unit = {
    for i <- 1 to times do write(key.replace("%n", i.toString), value.replace("%n", i.toString))
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
        case Some(multiget(key, times))        => multiget(key, times.toInt)
        case Some(multiput(key, value, times)) => multiput(key, value, times.toInt)
        case Some(waitForRes(flag))            => waitForOp = flag.toBoolean
        case Some("wait")                      => lock.synchronized { lock.wait() }
        case Some("exit")                      => running = false
        case None                              => running = false
        case other =>
          println("assuming put")
          write("key", "value")
      }
    }
    println(s"ended")
  }

  export dataManager.addLatentConnection

}
