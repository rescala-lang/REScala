package dtn

import sttp.capabilities.WebSockets
import sttp.client4.*
import sttp.client4.httpclient.HttpClientFutureBackend
import sttp.model.Uri

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CompatCode {
  val backend: GenericBackend[Future, WebSockets] = HttpClientFutureBackend()

  def uget(uri: Uri): Future[String] = backend.send(basicRequest.get(uri).response(asStringAlways)).map(x => x.body)
}
