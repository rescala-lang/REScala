package dtn

import dtn.Dtn7RsWsConn
//import dtn.Bundle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import dtn2.{Bundle, PreviousNodeBlock, BundleAgeBlock, HopCountBlock, PayloadBlock, UnknownBlock, given_Decoder_Bundle, given_Encoder_Bundle}

import java.nio.file.{Files, Paths}

import io.bullet.borer.Cbor


@main def run(): Unit = {
  
  val bundle_bytes = Files.readAllBytes(Paths.get("simple_bundle"))

  val bundle = Cbor.decode(bundle_bytes).to[Bundle].value

  val new_bundle_bytes = Cbor.encode(bundle).toByteArray

  val new_bundle = Cbor.decode(new_bundle_bytes).to[Bundle].value

  println(new_bundle)

  for (block <- new_bundle.other_blocks) {
    block match
      case x: PayloadBlock => println(s"payload contents: ${x.data}")
      case x: PreviousNodeBlock => println(s"previous node contents: ${x.previous_node_id}")
      case x: BundleAgeBlock => println(s"bundle age contents: ${x.age_milliseconds}")
      case x: HopCountBlock => println(s"hop count contents: ${x.hop_count}")
      case x: UnknownBlock => println(s"unknown block contents: ${x.data}")
  }
  

  /*
  Dtn7RsWsConn.create(3000).map(conn => {
    println(s"connected to node: ${conn.nodeId.get}")

    // flush receive forever
    def flush_receive(): Future[Unit] = {
      conn.receiveBundle().flatMap(bundle => {
        println(s"received bundle: $bundle")
        flush_receive()
      })
    }
    flush_receive()

    val cRDTGroupEndpoint = "dtn://global/~crdt/app1"

    conn.registerEndpointAndSubscribe(cRDTGroupEndpoint).flatMap(_ => {
      val bundle = Bundle.createWithUTF8TextAsData(
        src = cRDTGroupEndpoint,
        dst = cRDTGroupEndpoint,
        strng = "Hello World",
        lifetime = 3600 * 24 * 1000,
        delivery_notification = false
      )

      println(s"start sending bundle with text: ${bundle.getDataAsUTF8Text}")
      conn.sendBundle(bundle)
    })
  })
  
  while (true) {
    Thread.sleep(200)
  }
  */
}
