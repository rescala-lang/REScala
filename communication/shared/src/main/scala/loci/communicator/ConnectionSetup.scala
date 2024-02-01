package loci
package communicator

import scala.util.{Success, Try}

sealed trait ConnectionSetup[+P <: ProtocolCommon]

trait Connector[+P <: ProtocolCommon] extends ConnectionSetup[P] {
  final def connect(failureReporter: Throwable => Unit = logging.reportException)
      (handler: Try[Connection[P]] => Unit): Unit = synchronized {
    val connected = Notice.Steady[Try[Connection[P]]](failureReporter)
    connected.notice foreach handler
    connect(connected)
  }

  protected type Connected[-C <: ProtocolCommon] = Notice.Steady.Source[Try[Connection[C]]]

  protected def connect(connectionEstablished: Connected[P]): Unit
}

trait Listener[+P <: ProtocolCommon] extends ConnectionSetup[P] {
  final def startListening(failureReporter: Throwable => Unit = logging.reportException)
      (handler: Try[Connection[P]] => Unit): Try[Listening] = synchronized {
    val connected = Notice.Stream[Try[Connection[P]]](failureReporter)
    connected.notice foreach handler
    startListening(connected) map { listening =>
      new Listening {
        def stopListening() = Listener.this synchronized { listening.stopListening() }
      }
    }
  }

  protected type Connected[-C <: ProtocolCommon] = Notice.Stream.Source[Try[Connection[C]]]

  protected def startListening(connectionEstablished: Connected[P]): Try[Listening]

  final def firstConnection: Connector[P] = new Connector[P] {
    override protected type Connected[-C <: ProtocolCommon] = Notice.Steady.Source[Try[Connection[C]]]

    protected def connect(connectionEstablished: Connected[P]): Unit = {
      var firstConnection: Connection[P] = null
      var listening: Try[Listening] = null

      listening = startListening(connectionEstablished.failureReporter) {
        case success @ Success(connection) =>
          if (connectionEstablished.trySet(success)) {
            connection.closed foreach { _ =>
              if (listening != null)
                listening foreach { _.stopListening() }
            }
            if (listening != null && !connection.open)
              listening foreach { _.stopListening() }
            firstConnection = connection
          }
          else
            connection.close()

        case failure =>
          connectionEstablished.trySet(failure)
      }

      if (firstConnection != null && !firstConnection.open)
        listening foreach { _.stopListening() }
    }
  }
}

trait Listening {
  def stopListening(): Unit
}
