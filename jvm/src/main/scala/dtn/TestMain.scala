package dtn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import java.nio.file.{Files, Paths}

import io.bullet.borer.{Cbor, Json, Codec}


@main def run(): Unit = {
  /*

  val bar = Packet.Error(reason = "abc")

  val encoded_bar = Json.encode(bar).toByteArray

  val decode_bar = Json.decode(encoded_bar).to[Packet].value

  println(s"${Json.encode(decode_bar).toUtf8String}")

  
  val bundle_bytes = Files.readAllBytes(Paths.get("simple_bundle"))

  val bundle = Cbor.decode(bundle_bytes).to[Bundle].value

  val new_bundle_bytes = Cbor.encode(bundle).toByteArray

  val new_bundle = Cbor.decode(new_bundle_bytes).to[Bundle].value


  println(bundle)

  println("\n\n")

  println(new_bundle)

  println("\n\n")

  for (block <- new_bundle.other_blocks) {
    block match
      case x: PayloadBlock => println(s"payload utf8 contents: ${x.payload_as_utf8}")
      case x: PreviousNodeBlock => println(s"previous node contents: ${x.previous_node_id}")
      case x: BundleAgeBlock => println(s"bundle age contents: ${x.age_milliseconds}")
      case x: HopCountBlock => println(s"hop count contents: ${x.hop_count}")
      case x: UnknownBlock => println(s"unknown block contents: ${x.data}")
  }

  println("\n\n")

  for (_ <- 0 to 10) {
    println(s"now: ${CreationTimestamp.NOW}")
  }


  val own_bundle: Bundle = BundleCreation.createBundleUTF8("hey there", "dtn://global/~crdt/app1", "dtn://global/~crdt/app1")

  print("\n\n")
  print(own_bundle)

  */

  WSEroutingClient.create(3000).map(conn =>

    def flush_receive(): Future[Unit] = {
      conn.receivePacket().flatMap(packet => {
        println(s"received packet: $packet")
        flush_receive()
      })
    }
    flush_receive().recover(throwable => println(throwable))
  )

  while (true) {
    Thread.sleep(200)
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
