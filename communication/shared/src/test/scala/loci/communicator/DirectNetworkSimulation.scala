package loci
package communicator

import DirectConnectionSimulation.SimulationProtocol

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Random, Success}

object DirectConnectionSimulation {
  type SimulationProtocol = messaging.ConnectionsBase.Protocol
}

trait DirectConnectionSimulation {
  protected val random: Random

  protected var deferred: Boolean

  protected var locked = false

  protected val events = mutable.ListBuffer.empty[(DirectConnection, () => Unit)]

  protected def evaluateEvents() =
    if (!deferred && !locked) {
      locked = true
      while (events.nonEmpty) {
        val (connection, _) = events(random.nextInt(events.size))
        val index = events indexWhere {
          case (elementConnection, _) => elementConnection == connection
        }

        val (_, event) = events.remove(index)
        event()
      }
      locked = false
    }

  def run() = {
    deferred = false
    evaluateEvents()
  }

  class DirectConnection(connectionSetup: ConnectionSetup[SimulationProtocol])
      extends Connection[SimulationProtocol] {
    var connection: DirectConnection = _
    var isOpen = true
    val doClosed = Notice.Steady[Unit](ExecutionContext.defaultReporter)
    val doReceive = Notice.Stream[MessageBuffer](ExecutionContext.defaultReporter)

    val protocol = new Protocol with SetupInfo with SecurityInfo with SymmetryInfo with Bidirectional {
      val setup = connectionSetup
      val encrypted = false
      val integrityProtected = false
      val authenticated = false
    }
    val closed = doClosed.notice
    val receive = doReceive.notice

    def open = isOpen

    def close() = {
      isOpen = false
      doClosed.set()
      events += this -> { () =>
        connection.isOpen = false
        connection.doClosed.set()
      }
      evaluateEvents()
    }

    def send(data: MessageBuffer) = {
      events += this -> { () => connection.doReceive.fire(data) }
      evaluateEvents()
    }
  }
}

class NetworkListener(
  protected var deferred: Boolean = false,
  protected val seed: Int = 0)
    extends DirectConnectionSimulation with Listener[SimulationProtocol] {

  protected val random = new Random(seed.toLong)

  protected val connected = mutable.ListBuffer.empty[Connected[SimulationProtocol]]

  def createConnector() = new Connector[SimulationProtocol] {
    protected def connect(connectionEstablished: Connected[SimulationProtocol]) = {
      val connection0 = new DirectConnection(NetworkListener.this)
      val connection1 = new DirectConnection(this)
      connection0.connection = connection1
      connection1.connection = connection0
      connected foreach { _.fire(Success(connection0)) }
      connectionEstablished.set(Success(connection1))
    }
  }

  protected def startListening(connectionEstablished: Connected[SimulationProtocol]) = {
    connected += connectionEstablished
    Success(new Listening {
      def stopListening() = { }
    })
  }
}

class NetworkConnector(
  protected var deferred: Boolean = false,
  protected val seed: Int = 0)
    extends DirectConnectionSimulation {

  protected val random = new Random(seed.toLong)

  val first = new Connector[SimulationProtocol] {
    protected def connect(connectionEstablished: Connected[SimulationProtocol]) =
      connectionEstablished.set(Success(connection0))
  }

  val second = new Connector[SimulationProtocol] {
    protected def connect(connectionEstablished: Connected[SimulationProtocol]) =
      connectionEstablished.set(Success(connection1))
  }

  protected val connection0: DirectConnection = new DirectConnection(first)
  protected val connection1: DirectConnection = new DirectConnection(second)
  connection0.connection = connection1
  connection1.connection = connection0
}
