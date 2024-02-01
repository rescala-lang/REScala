package loci
package registry

import transmitter._

import java.util.concurrent.ConcurrentHashMap

import scala.util.{Failure, Try}

class Bindings[A <: AbstractionRef](
    request: (A, MessageBuffer) => Unit,
    respond: (A, Try[MessageBuffer]) => Unit) {

  private val bindings = new ConcurrentHashMap[
    String, (MessageBuffer, A) => Try[MessageBuffer]]

  private val responseHandlers = new ConcurrentHashMap[
    Channel, Notice.Steady.Source[Try[MessageBuffer]]]

  def bind[T, R](binding: Binding[T, R])(function: RemoteRef => T): Unit =
    if (bindings.putIfAbsent(binding.name, binding.dispatch(function, _, _)) != null)
      throw new RemoteAccessException(s"binding for the given name already exists: ${binding.name}")

  def unbind(name: String): Unit =
    bindings.remove(name)

  def lookup[T, R](binding: Binding[T, R], createAbstraction: () => A): R =
    binding.call(createAbstraction) { (message, abstraction) =>
      val channel = abstraction.channel
      val response = Notice.Steady[Try[MessageBuffer]]

      responseHandlers.put(channel, response)

      if (!abstraction.remote.connected)
        channelsClosed()

      logging.trace(s"sending remote access for $abstraction")

      request(abstraction, message)
      response.notice
    }

  def processRequest(
      message: MessageBuffer, name: String, abstraction: A): Unit = {
    logging.trace(s"handling remote access for $abstraction")

    val value = bindings get name match {
      case null =>
        Failure(new RemoteAccessException(s"request for $abstraction could not be dispatched"))
      case dispatch =>
        dispatch(message, abstraction)
    }

    value.failed foreach { exception =>
      logging.warn("local exception upon remote access propagated to remote instance", exception)
    }

    logging.trace(s"sending remote access response for $abstraction")

    respond(abstraction, value)
  }

  def processResponse(
      message: Try[MessageBuffer], name: String, abstraction: A): Unit =
    responseHandlers.remove(abstraction.channel) match {
      case null =>
        logging.warn(s"unprocessed message for $abstraction [no handler]: $message")
      case response =>
        response.set(message)
    }

  def channelsClosed(): Unit = {
    val iterator = responseHandlers.entrySet.iterator
    while (iterator.hasNext) {
      val entry = iterator.next()
      if (!entry.getKey.open) {
        entry.getValue.trySet(Failure(new RemoteAccessException(RemoteAccessException.RemoteDisconnected)))
        iterator.remove()
      }
    }
  }
}
