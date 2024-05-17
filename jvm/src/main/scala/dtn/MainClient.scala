package dtn

import kofre.time.{Dots, Dot, Time}
import kofre.base.Uid
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import io.bullet.borer.Cbor


/* 
  A collection of main methods that are used either in direct testing or simulation
*/


@main def send_ping_to_node4000_from_3000(): Unit = send_ping_to_node4000("127.0.0.1", 3000)

@main def send_one_rdt_package_from_3000(): Unit = send_one_rdt_package("127.0.0.1", 3000)
@main def send_one_rdt_package_from_4000(): Unit = send_one_rdt_package("127.0.0.1", 4000)

@main def send_continuous_rdt_packages_from_3000(): Unit = send_continuous_rdt_packages("127.0.0.1", 3000)

@main def send_one_rdt_package_with_random_dots_and_checker_from_3000(): Unit = send_one_rdt_package_with_random_dots_and_checker("127.0.0.1", 3000)



def send_ping_to_node4000(host: String, port: Int): Unit = {
  WSEndpointClient(host, port).flatMap(client => {
    // flush receive forever
    def flush_receive(): Future[Bundle] = {
      client.receiveBundle().flatMap(bundle => {
        println(s"received bundle: $bundle")
        flush_receive()
      })
    }
    flush_receive().recover(throwable => println(throwable))

    val bundle: Bundle = BundleCreation.createBundleUTF8(
      utf8_payload = "Ping",
      full_destination_uri = "dtn://node4000/incoming",
      full_source_uri = Endpoint.NONE_ENDPOINT.full_uri
    )

    client.sendBundle(bundle)
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}


def send_one_rdt_package(host: String, port: Int): Unit = {
  val global_rdt_testendpoint: String = "dtn://global/~rdt/testapp"
  def node_rdt_testendpoint(nodeid: String): String = s"${nodeid}rdt/testapp"

  WSEndpointClient(host, port)
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
    }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}


def send_continuous_rdt_packages(host: String, port: Int): Unit = {
  val global_rdt_testendpoint: String = "dtn://global/~rdt/testapp"
  def node_rdt_testendpoint(nodeid: String): String = s"${nodeid}rdt/testapp"

  WSEndpointClient(host, port)
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



def send_one_rdt_package_with_random_dots_and_checker(host: String, port: Int): Unit = {
  val global_rdt_testendpoint: String = "dtn://global/~rdt/testapp"
  def node_rdt_testendpoint(nodeid: String): String = s"${nodeid}rdt/testapp"

  val convergenceCheckerClient = DotsConvergenceClient("127.0.0.1", 5000)

  // generate dots once
  val myUid = Uid.gen()
  var dots: Dots = Dots.empty
  dots = dots.add(Dot(myUid, Time.current()))


  WSEndpointClient(host, port)
    .flatMap(client => client.registerEndpointAndSubscribe(global_rdt_testendpoint))
    .flatMap(client => client.registerEndpointAndSubscribe(node_rdt_testendpoint(client.nodeId.get)))
    .flatMap(client => {
      // flush receive forever and send dots to checker
      def flush_receive(): Future[Bundle] = {
        client.receiveBundle().flatMap(bundle => {
          println(s"received bundle: $bundle")
          
          bundle.other_blocks.collectFirst({
            case x: RdtMetaBlock => x
          }) match
            case None => println("did not contain rdt-meta data")
            case Some(rdt_meta_block) => {
              dots = dots.merge(rdt_meta_block.dots)
              convergenceCheckerClient.send(dots)
              println(s"merged rdt-meta data, sent to checker, new dots: $dots")
            }
          
          flush_receive()
        })
      }
      flush_receive().recover(throwable => println(throwable))

      // send one bundle with the generated dots
      val bundle: Bundle = BundleCreation.createBundleRdt(
        data = Array(),
        dots = dots,
        node = Endpoint.createFrom(client.nodeId.get),
        full_destination_uri = global_rdt_testendpoint,
        full_source_uri = node_rdt_testendpoint(client.nodeId.get)
      )

      println(s"sending bundle with new dots: $dots")
      client.sendBundle(bundle)

      Future(client)
    }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}


