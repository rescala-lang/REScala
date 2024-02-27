package dtn

import kofre.base.Lattice
import rescala.core.InitialChange
import rescala.default.scheduler
import rescala.default.*

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import boopickle.Default._


object CrdtConnector {
  private val ws = Dtn7RsWsConn()

  println(s"connected to DTN node: ${ws.nodeId}")
  if (ws.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

  val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

  ws.registerEndpointAndSubscribe(cRDTGroupEndpoint)

  private var onChangedUpdateFunc: Array[Byte] => Unit = x => {}

  private var receiverThreadKeepRunning: Boolean = true
  private val receiverThread = new Thread {
    override def run(): Unit = {
      while (receiverThreadKeepRunning) {
        // update crdt on signals received through the dtn network
        val bundle: Bundle = Await.result(ws.receiveBundle(), Duration.Inf)

        onChangedUpdateFunc(bundle.getDataAsBytes)
      }
    }
  }

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

    // delay receiver start until actual received-updates-processing method is set
    receiverThread.start()
  }

  def disconnect(): Unit = {
    receiverThreadKeepRunning = false
    ws.disconnect()
  }
}
