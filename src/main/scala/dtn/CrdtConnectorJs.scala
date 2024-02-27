package dtn

import kofre.base.Lattice
import rescala.core.InitialChange
import rescala.default.scheduler
import rescala.default.*

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import boopickle.Default._


object CrdtConnectorJs {
  private val ws = Dtn7RsWsConnJs()

  println(s"connected to DTN node: ${ws.nodeId}")
  if (ws.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

  val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

  ws.registerEndpointAndSubscribe(cRDTGroupEndpoint)

  private var onChangedUpdateFunc: Array[Byte] => Unit = x => {}

  def connect[A: Lattice](signal: Signal[A]): Unit = {
    // send signal updates through the dtn network
    signal observe ((x: A) => {
      val payload: Array[Byte] = Utility.toByteArray(Pickle.intoBytes(x))
      val bundle: Bundle = Bundle.createWithBytesAsData(
        src = cRDTGroupEndpoint,
        dst = cRDTGroupEndpoint,
        bytes = payload
      )
      ws.sendBundle(bundle)
    })

    // push received updates into the crdt
    onChangedUpdateFunc = (v: Array[Byte]) => {
      val newValue: A = Unpickle[A].fromBytes(Utility.toByteBuffer(v))
      
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

  def runForever(): Unit = {
    while (true) {
      // update crdt on signals received through the dtn network
      val bundle: Bundle = ws.receiveBundle()

      onChangedUpdateFunc(bundle.getDataAsBytes)
    }
  }
}
