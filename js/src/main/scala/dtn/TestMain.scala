package dtn

import dtn.Dtn7RsWsConn
import dtn.Bundle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//import dtn2.Bundle
//import dtn2.given_Decoder_Bundle

import java.nio.file.{Files, Paths}

import io.bullet.borer.Cbor


@main def run(): Unit = {
  /*
  val bundle_bytes = Files.readAllBytes(Paths.get("simple_bundle"))

  val bundle = Cbor.decode(bundle_bytes).to[Bundle].value

  println(bundle)
  */

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
}
