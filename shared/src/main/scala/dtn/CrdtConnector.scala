package dtn

import kofre.base.Lattice
import rescala.core.InitialChange
import rescala.default.scheduler
import rescala.default.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}


object CrdtConnector {
  private val cRDTGroupEndpoint: String = "dtn://global/~crdt/app1"

  private var ws: Option[Dtn7RsWsConn] = None

  private var onChangedUpdateFunc: Array[Byte] => Unit = x => {}
  
  def connectToWS(port: Int): Unit = {
    def receiveBundle(): Unit = {
      ws.get.receiveBundle().onComplete(bundle => {
        for (block <- bundle.get.other_blocks) {
          block match
            case PayloadBlock(block_type_code, block_number, block_processing_control_flags, crc_type, data) => onChangedUpdateFunc(block.data)
            case _ => {}
        }
        receiveBundle()
      })
    }
    
    Dtn7RsWsConn.create(port).flatMap(conn => {
      ws = Some(conn)
      ws.get.registerEndpointAndSubscribe(cRDTGroupEndpoint)
    }).onComplete(_ => receiveBundle())
  }

  def connectGraph[A: Lattice: JsonValueCodec](signal: Signal[A]): Unit = {
    // send signal updates through the dtn network
    signal observe ((x: A) => {
      val payload: Array[Byte] = writeToArray(x)
      val bundle: Bundle = Creation.createBundle(
        data = payload,
        full_destination_uri = cRDTGroupEndpoint,
        full_source_url = cRDTGroupEndpoint
      )
      ws match
        case Some(value) => value.sendBundle(bundle)
        case None => println("cannot send update. not connected. throwing away.")
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
}
