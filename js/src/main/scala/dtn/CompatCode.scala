package dtn

import sttp.client4.fetch.FetchBackend
import sttp.client4.GenericBackend
import sttp.capabilities.WebSockets
import scala.concurrent.Future


object CompatCode {
  def getBackend(): GenericBackend[Future, WebSockets] = FetchBackend()

  def exitWithMessage(message: String): Unit = println(message + " - IMPORTANT NOTE: not exiting due to scalajs not supporting sys.exit()")
}
