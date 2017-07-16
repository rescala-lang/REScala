package stateCrdts

import akka.actor._
import akka.pattern.ask
import java.net.InetAddress

import akka.util.Timeout
import rescala._
import stateCrdts.DistributionEngine._
import stateCrdts.LookupServer.{LookupMessage, RegisterMessage}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Handles distribution on the client side and provides a frontend for the programmer to interact with.
  */
class DistributionEngine(hostName: String = InetAddress.getLocalHost.getHostAddress, lookupServer: ActorRef) extends Actor {

  private var registry: Map[String, Set[ActorRef]] = Map().withDefaultValue(Set())
  var localVars: Map[String, Var[StateCRDT]] = Map()

  // fakes the existence of a server infrastructure
  // maps hostname to map of varName and value
  // TODO: make private
  //var cloudStorage: Map[String, Map[String, Any]] = Map().withDefaultValue(Map())

  def receive = {
    case Publish(varName, localVar) => publish(varName, localVar.asInstanceOf[Var[StateCRDT]])
    case UpdateMessage(varName, value, hostRef) =>
      sleep
      println(s"[$hostName] received value $value for $varName from ${hostRef.path.name}")
      localVars(varName).transform(_.merge(value)) // update value with merged crdt
    val newHosts = registry(varName) + hostRef
      registry += (varName -> newHosts) // add sender to registry
    case QueryMessage(varName) =>
      sleep
      sender ! UpdateMessage(varName, localVars(varName).now, self)
  }

  /**
    * Publishes a variable on this host to the other hosts under a given name.
    *
    * @param varName  The public name for the published variable
    * @param localVar the local Var to be published
    */
  private def publish(varName: String, localVar: Var[StateCRDT]): Unit = {
    // LookupServer Registration:
    implicit val timeout = Timeout(60.second)
    val crdt = localVar.now // retrieve current value of CRDT

    val sendMessage = lookupServer ? RegisterMessage(varName, self) // register this instance
    val registration = sendMessage andThen {
      case Success(v) => v match {
        case l: Set[ActorRef] => {
          println("[" + hostName + "] registered " + varName + " on lookup server!")
          registry += (varName -> l.filter(a => a != self))
          println("[" + hostName + "] Setting registry to " + l.filter(a => a != self))
        }
      }
      case Failure(_) => println("[" + hostName + "] Could not reach lookup server!")
    }
    Await.ready(registration, Duration.Inf) // await the answer of the lookupServer

    //set(varName, mergedCrdt) // update other hosts with new value

    println("Adding listener for " + localVar)
    localVar.changed += ((c) => {
      //println(s"[$hostName] $varName changed to $c")
      sendUpdates(varName, c)
    })

    localVars += (varName -> localVar) // save reference to the var

    // Query and udate all other hosts
    sendUpdates(varName, crdt)
    query(varName)
  }

  private def sendUpdates(varName: String, value: StateCRDT): Unit = {
    registry(varName).foreach(a => a ! UpdateMessage(varName, value, self))
  }

  private def query(varName: String): Unit = {
    registry(varName).foreach(a => a ! QueryMessage(varName))
  }

  /*
  /**
    * Updates the value of a given variable on all hosts.
    *
    * @param varName The public name of the published variable
    * @param value   The new value for the published variable
    */
  def set(varName: String, value: StateCRDT): Unit =
    registry(varName).filter((hostname) => hostname != host) // get all hosts differing from this one
      .foreach((hostname) => {
      cloudStorage = cloudStorage +
        (hostname -> (cloudStorage(hostname) +
          (varName -> value.payload))) // add new value to cloudStorage

      val localVar = localVars(hostname)(varName).asInstanceOf[Var[StateCRDT]]
      val localPayload = localVar.now.payload
      if (localVar.now.payload != value.payload) { // only update local instance if payload changed
        localVar.set(localVar.now.merge(value))
        println("[" + hostname + "] Setting " + varName + "(" + localVar.now.asInstanceOf[CIncOnlyCounter].id + ") from " + localPayload + " to " + value.payload)
      }
      //Thread sleep 2000
    })
    */

  /**
    * Pull all hosts and return the new merged value for a given name
    */
  /* TODO: implement or remove
  def update(varName: String, localVal: StateCRDT) =
    registry(varName).foldLeft(localVal)((newValue, hostname) =>
      newValue.merge(newValue.fromPayload(cloudStorage(hostname)(varName).asInstanceOf[newValue.payloadType])).asInstanceOf[StateCRDT])
  */
}

object DistributionEngine {
  def props(host: String, lookupServer: ActorRef): Props = Props(new DistributionEngine(host, lookupServer))

  final case class Publish(varName: String, localVar: Var[_ <: StateCRDT])

  final case class UpdateMessage(varName: String, value: StateCRDT, hostRef: ActorRef)

  final case class QueryMessage(varName: String)

  def host = InetAddress.getLocalHost // hostname + IP
  def ip = InetAddress.getLocalHost.getHostAddress

  /**
    * Generates unique identifiers based on the current Hostname, IP address and a UUID based on the current system time.
    *
    * @return A new unique identifier (e.g. hostname/127.0.0.1::1274f9fe-cdf7-3f10-a7a4-33e8062d7435)
    */
  def genId: String = host + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)
}

class LookupServer extends Actor {
  // maps varName to list of sharing hosts
  private var registry: Map[String, Set[ActorRef]] = Map().withDefaultValue(Set())

  /**
    * Registers a new shared variable to the lookup server and returns a list of other hosts sharing this variable.
    *
    * @param varName The name of the public variable
    * @param hostRef The hostname of the server to be registered
    * @return A list of other hosts sharing this variable
    */
  def register(varName: String, hostRef: ActorRef): Set[ActorRef] = {
    val newHosts = registry(varName) + hostRef
    registry += (varName -> newHosts)
    registry(varName)
  }

  /**
    * Returns all hosts for this variable name
    *
    * @param name name of the shared variable
    * @return list of hosts sharing the variable
    *
    */
  def lookup(name: String): Set[ActorRef] = registry(name)

  def receive = {
    case RegisterMessage(varName, host) => {
      sleep
      println("[LookupServer] reveived register message for " + varName + " from " + host)
      sender ! register(varName, host) // register sender and send return new list of hosts
    } // TODO: implement
    case LookupMessage(varName) => {
      sleep
      sender ! lookup(varName)
    }
  }


}

object LookupServer {
  def props: Props = Props[LookupServer]

  final case class RegisterMessage(varName: String, host: ActorRef)

  final case class LookupMessage(varName: String)

}