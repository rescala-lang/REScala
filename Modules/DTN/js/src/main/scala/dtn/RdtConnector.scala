package dtn

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.Lattice
import reactives.core.InitialChange
import reactives.default.*

import scala.concurrent.ExecutionContext.Implicits.global

object RdtConnector {
  private val cRDTGroupEndpoint: String = "dtn://global/~rdt/app1"

  private var ws: Option[WSEndpointClient] = None

  private var onChangedUpdateFunc: Array[Byte] => Unit = x => {}

  def connectToWS(host: String, port: Int): Unit = {
    def receiveBundle(): Unit = {
      ws.get.receiveBundle().onComplete(bundle => {
        for block <- bundle.get.other_blocks do {
          block match
            case PayloadBlock(block_type_code, block_number, block_processing_control_flags, crc_type, data) =>
              onChangedUpdateFunc(block.data)
            case _ =>
        }
        receiveBundle()
      })
    }

    WSEndpointClient(host, port).flatMap(conn => {
      ws = Some(conn)
      ws.get.registerEndpointAndSubscribe(cRDTGroupEndpoint)
    }).onComplete(_ => receiveBundle())
  }

  def connectGraph[A: Lattice: JsonValueCodec](signal: Signal[A]): Unit = {
    // send signal updates through the dtn network
    signal observe ((x: A) => {
      val payload: Array[Byte] = writeToArray(x)
      val bundle: Bundle = BundleCreation.createBundle(
        data = payload,
        full_destination_uri = cRDTGroupEndpoint,
        full_source_uri = cRDTGroupEndpoint
      )
      ws match
        case Some(value) =>
          value.sendBundle(bundle).printError()
        case None => println("cannot send update. not connected. throwing away.")
    })

    // push received updates into the crdt
    onChangedUpdateFunc = (v: Array[Byte]) => {
      val newValue: A = readFromArray[A](v)

      reactives.SelectedScheduler.candidate.scheduler.forceNewTransaction(signal) {
        admissionTicket =>
          admissionTicket.recordChange(new InitialChange {
            override val source: signal.type = signal

            override def writeValue(b: source.Value, v: source.Value => Unit): Boolean = {
              val merged = b.map(Lattice[A].merge(_, newValue)).asInstanceOf[source.Value]
              if merged != b then {
                v(merged)
                true
              } else false
            }
          })
      }
    }
  }
}
