package dtn

import sttp.model.Uri
import sttp.client4.fetch.FetchBackend
import sttp.client4.GenericBackend
import sttp.client4.*
import sttp.capabilities.WebSockets
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object CompatCode {
  val backend: GenericBackend[Future, WebSockets] = FetchBackend()

  def uget(uri: Uri): Future[String] = backend.send(basicRequest.get(uri).response(asStringAlways)).map(x => x.body)
}
