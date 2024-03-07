package dtn

import kofre.base.Lattice
import rescala.core.InitialChange
import rescala.default.scheduler
import rescala.default.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}


object CrdtConnector {
  private val cRDTGroupEndpoint: String = "dtn://global/crdt/~app1"

  private var ws: Option[Dtn7RsWsConn] = None

  private var onChangedUpdateFunc: Array[Byte] => Unit = x => {}

  def initializeObj(): Future[Unit] = {
    Dtn7RsWsConn.create().flatMap(conn => {
      ws = Option(conn)

      println(s"connected to DTN node: ${ws.get.nodeId.get}")
      if (ws.get.nodeId.get.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

      ws.get.registerEndpointAndSubscribe(cRDTGroupEndpoint)
    })
  }

  def connectGraph[A: Lattice: JsonValueCodec](signal: Signal[A]): Unit = {
    // send signal updates through the dtn network
    signal observe ((x: A) => {
      val payload: Array[Byte] = writeToArray(x)
      val bundle: Bundle = Bundle.createWithBytesAsData(
        src = cRDTGroupEndpoint,
        dst = cRDTGroupEndpoint,
        bytes = payload
      )
      println("start sending")
      ws.get.sendBundle(bundle).onComplete(_ => println("finished sending"))
    })

    // push received updates into the crdt
    onChangedUpdateFunc = (v: Array[Byte]) => {
      val newValue: A = readFromArray[A](v)
      
      scheduler.forceNewTransaction(signal) {
        admissionTicket => admissionTicket.recordChange(new InitialChange {
          override val source: signal.type = signal

          override def writeValue(b: source.Value, v: source.Value => Unit): Boolean = {
            val merged = b.map(Lattice[A].merge(_, newValue)).asInstanceOf[source.Value]
            if (merged != b) {
              v(merged)
              true
            } else false
          }
        })
      }
    }
  }

  def startReceiving(): Unit = {
    println("start receiving")
    // update crdt on signals received through the dtn network
    ws.get.receiveBundle().onComplete(bundle => {
      println("got a bundle")
      onChangedUpdateFunc(bundle.get.getDataAsBytes)
      println("pushed into CRDT")
      startReceiving()
    })
  }
}
