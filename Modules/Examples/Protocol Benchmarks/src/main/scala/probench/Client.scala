package probench

import probench.data.{ClientNodeState, DataManager, KVOperation, Request, Response}
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.contextual.CausalQueue
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

import scala.io.StdIn.readLine
import scala.util.matching.Regex

class Client {

  given localUid: LocalUid = LocalUid.gen()
  private val dataManager  = DataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onStateChange)

  private val lock                       = new Object()
  private var currentOp: Option[Request] = None

  val get: Regex = """get (\w+)""".r
  val put: Regex = """put (\w+) (\w+)""".r

  private def onStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    for {
      op <- currentOp
      CausalQueue.QueueElement(res@Response(req, _), _, _) <- newState.responses.data.values if req == op
    } {
      println(res.response)

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

    dataManager.transform { current =>
      current.mod(it => it.copy(requests = it.requests.mod(_.enqueue(req))))
    }

    lock.synchronized {
      lock.wait()
    }
  }

  def read(key: String): Unit = {
    handleOp(KVOperation.Read(key))
  }

  def write(key: String, value: String): Unit = {
    handleOp(KVOperation.Write(key, value))
  }

  def startCLI(): Unit = {
    while true do {
      print("client> ")
      val line = readLine()

      line match {
        case get(key)        => read(key)
        case put(key, value) => write(key, value)
        case "exit"          => System.exit(0)
        case _               => println(s"Error parsing: $line")
      }
    }
  }

  export dataManager.addLatentConnection

}
