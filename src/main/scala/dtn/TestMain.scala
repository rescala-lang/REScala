package dtn

import dtn.Dtn7RsWsConn
import dtn.Dtn7RsWsConnJs

import scala.concurrent.ExecutionContext.Implicits.global

@main def run(): Unit = {
  if (true) {
    val conn = Dtn7RsWsConn()

    println(s"connected to node: ${conn.nodeId}")

    if (conn.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

    val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

    conn.registerEndpointAndSubscribe(cRDTGroupEndpoint)

    val bundle = Bundle.createWithUTF8TextAsData(
      src = cRDTGroupEndpoint,
      dst = cRDTGroupEndpoint,
      strng = "Hello World",
      lifetime = 3600 * 24 * 1000,
      delivery_notification = false
    )
    println(s"start sending bundle with text: ${bundle.getDataAsUTF8Text}")
    conn.sendBundle(bundle).onComplete(response => println(s"finished sending bundle, response: ${response.get}"))

    println(s"start receiving bundle")
    conn.receiveBundle().onComplete(response => {
      println(s"received bundle with text: ${response.get.getDataAsUTF8Text}")
      conn.disconnect()
    })
  } else {
    val conn = Dtn7RsWsConnJs()

    println(s"connected to node: ${conn.nodeId}")

    if (conn.nodeId.startsWith("ipn")) throw Exception("DTN mode IPN is unsupported by this client")

    val cRDTGroupEndpoint = "dtn://global/crdt/~app1"

    conn.registerEndpointAndSubscribe(cRDTGroupEndpoint)

    val bundle = Bundle.createWithUTF8TextAsData(
      src = cRDTGroupEndpoint,
      dst = cRDTGroupEndpoint,
      strng = "Hello World",
      lifetime = 3600 * 24 * 1000,
      delivery_notification = false
    )
    println(s"start sending bundle with text: ${bundle.getDataAsUTF8Text}")
    val responseStr: String = conn.sendBundle(bundle)
    println(s"finished sending bundle, response: ${responseStr}")

    println(s"start receiving bundle")
    val responseBundle: Bundle = conn.receiveBundle()
    println(s"received bundle with text: ${responseBundle.getDataAsUTF8Text}")
    conn.disconnect()
  }
}
