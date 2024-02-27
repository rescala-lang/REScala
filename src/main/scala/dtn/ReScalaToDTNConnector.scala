package dtn

import kofre.base.Lattice
import rescala.core.InitialChange
import rescala.default.scheduler
import rescala.default.*

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.concurrent.Await
import scala.concurrent.duration.Duration


object ReScalaToDTNConnector {
  private val ws = Client.createWS

  println(s"connected to DTN node: ${Client.nodeId}")
  if (Client.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

  val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

  ws.registerEndpointAndSubscribe(cRDTGroupEndpoint)

  private var onChangedUpdateFunc: Array[Byte] => Unit = x => {}

  private var receiverThreadKeepRunning: Boolean = true
  private val receiverThread = new Thread {
    override def run(): Unit = {
      while (receiverThreadKeepRunning) {
        // update crdt on signals received through the dtn network
        val bundle: Bundle = Await.result(ws.receiveBundle(), Duration.Inf)

        Serialization.deserialise(bundle.getDataAsBytes)
      }
    }
  }

  def connect[A: Lattice](signal: Signal[A]): Unit = {
    // send signal updates through the dtn network
    signal observe ((x: A) => {
      val payload: Array[Byte] = Serialization.serialise(x)
      val bundle: Bundle = Bundle.createWithBytesAsData(
        src = cRDTGroupEndpoint,
        dst = cRDTGroupEndpoint,
        bytes = payload
      )
      ws.sendBundle(bundle)
    })

    // push received updates into the crdt
    onChangedUpdateFunc = (v: Array[Byte]) => {
      val newValue: A = Serialization.deserialise(v).asInstanceOf[A]

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


object Serialization {

  def serialise(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  def deserialise(bytes: Array[Byte]): Any = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close()
    value
  }
}
