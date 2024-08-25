package dtn

import sttp.capabilities.WebSockets
import sttp.client4.*
import sttp.client4.fetch.FetchBackend
import sttp.model.Uri

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object CompatCode {
  val backend: GenericBackend[Future, WebSockets] = FetchBackend()

  def uget(uri: Uri): Future[String] = backend.send(basicRequest.get(uri).response(asStringAlways)).map(x => x.body)
}

extension [U >: Unit](fut: Future[U])
  def recoverAndLog(): Future[U] = {
    fut.recover(_.printStackTrace())
  }

extension [U >: Unit](x: Try[U])
  def recoverAndLog(): Try[U] = {
    x.recover(_.printStackTrace())
  }

extension [E <: Exception](e: E)
  def log(): Unit = {
    e.printStackTrace()
  }
