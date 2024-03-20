package dtn

import sttp.client4.httpclient.HttpClientFutureBackend
import sttp.client4.GenericBackend
import scala.concurrent.Future
import sttp.capabilities.WebSockets


object CompatCode {
  def getBackend(): GenericBackend[Future, WebSockets] = HttpClientFutureBackend()
}
