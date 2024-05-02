package dtn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import java.nio.file.{Files, Paths}

import io.bullet.borer.{Cbor, Json, Codec}
import dtn.routers.{DirectRouter, EpidemicRouter, RdtDotsRouter}
import kofre.time.{Dots, Dot, Time}
import kofre.base.Uid


@main def run(): Unit = {

  var dotmap: Map[String, Dots] = Map()

  dotmap += ("abc" -> Dots.empty)

  println(dotmap)


  var dots: Dots = Dots.empty

  dots = dots.add(Dot(Uid.gen(), 123))

  val encDots = Cbor.encode(dots).toByteArray

  dots = Cbor.decode(encDots).to[Dots].value

  println(dots)


  val e = Endpoint.createFrom("dtn://node1/")

  println(e)
  println(e.extract_node_endpoint())
  println(e.extract_node_name())
  println(e.extract_endpoint_id())

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


@main def start_rdtdots_routing(): Unit = {
  RdtDotsRouter.create(3000).flatMap(router => {
    router.start_receiving()
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}


@main def start_epidemic_routing(): Unit = {
  EpidemicRouter.create(3000).flatMap(router => {
    router.start_receiving()
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}

@main def start_direct_routing(): Unit = {
  DirectRouter.create(3000).flatMap(router => {
    router.start_receiving()
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}

@main def send_direct_package(): Unit = {
  WSEndpointClient.create(3000).flatMap(client => {
    // flush receive forever
    def flush_receive(): Future[Bundle] = {
      client.receiveBundle().flatMap(bundle => {
        println(s"received bundle: $bundle")
        flush_receive()
      })
    }
    flush_receive().recover(throwable => println(throwable))

    // send one bundle to //node4000/
    val bundle: Bundle = BundleCreation.createBundleUTF8(
      utf8_payload = "Ping",
      full_destination_uri = "dtn://node5000/incoming",
      full_source_uri = Endpoint.NONE_ENDPOINT.full_uri
    )

    client.sendBundle(bundle)
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}

@main def send_rdt_test_packages(): Unit = {
  val global_rdt_testendpoint: String = "dtn://global/~rdt/testapp"
  def node_rdt_testendpoint(nodeid: String): String = s"$nodeid~rdt/testapp"

  WSEndpointClient.create(3000)
    .flatMap(client => client.registerEndpointAndSubscribe(global_rdt_testendpoint))
    .flatMap(client => client.registerEndpointAndSubscribe(node_rdt_testendpoint(client.nodeId.get)))
    .flatMap(client => {
      val myUid = Uid.gen()
      var dots: Dots = Dots.empty

      // flush receive forever
      def flush_receive(): Future[Bundle] = {
        client.receiveBundle().flatMap(bundle => {
          println(s"received bundle: $bundle")
          
          bundle.other_blocks.collectFirst({
            case x: RdtMetaBlock => x
          }) match
            case None => println("did not contain rdt-meta data")
            case Some(rdt_meta_block) => {
              dots = dots.merge(rdt_meta_block.dots)
              println(s"merged rdt-meta data, new dots: $dots")
            }
          
          flush_receive()
        })
      }
      flush_receive().recover(throwable => println(throwable))

      // send a bundle with new dots at regular interval
      while(true) {
        Thread.sleep(5000)

        dots = dots.add(Dot(myUid, Time.current()))

        val bundle: Bundle = BundleCreation.createBundleRdt(
          data = Array(),
          dots = dots,
          node = Endpoint.createFrom(client.nodeId.get),
          full_destination_uri = global_rdt_testendpoint,
          full_source_uri = node_rdt_testendpoint(client.nodeId.get)
        )

        println(s"sending bundle with new dots: $dots")
        client.sendBundle(bundle)
      }

      Future(client)
    }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}
