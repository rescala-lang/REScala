package loci
package communicator
package tcp

import scala.concurrent.duration.FiniteDuration

trait TCPSetupFactory
    extends ConnectionSetupFactory.Implementation[TCP]
    with ConnectionSetupParser
    with SimpleConnectionSetupProperties {
  val self: TCP.type = TCP

  val schemes = Seq("tcp")

  protected def properties(implicit props: ConnectionSetupFactory.Properties) =
    TCP.Properties()
      .set[FiniteDuration]("heartbeat-delay") { v => _.copy(heartbeatDelay = v) }
      .set[FiniteDuration]("heartbeat-timeout") { v => _.copy(heartbeatTimeout = v) }
      .set[Boolean]("no-delay") { v => _.copy(noDelay = v) }

  protected def listener(
      url: String, scheme: String, location: String, properties: TCP.Properties) =
    parse(location) match {
      case (Some(interface), Some(port)) => Some(TCP(port, interface, properties))
      case (None, Some(port)) => Some(TCP(port, properties))
      case _ => None
    }

  protected def connector(
      url: String, scheme: String, location: String, properties: TCP.Properties) =
    parse(location) match {
      case (Some(host), Some(port)) => Some(TCP(host, port, properties))
      case _ => None
    }

  protected def parse(location: String): (Option[String], Option[Int]) =
    try {
      val index = location lastIndexOf ':'
      if (index != -1) {
        val hostInterface = location.substring(0, index)
        val port = Some(location.substring(index + 1).toInt)
        if (hostInterface.nonEmpty &&
            hostInterface.head == '[' && hostInterface.last == ']')
          (Some(hostInterface.substring(1, hostInterface.length - 1)), port)
        else
          (Some(hostInterface), port)
      }
      else
        (None, Some(location.toInt))
    }
    catch {
      case _: NumberFormatException =>
        (None, None)
    }
}
