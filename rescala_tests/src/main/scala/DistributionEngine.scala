import java.net.InetAddress
import rescala._

/**
  * Handles distribution on the client side and provides a frontend for the programmer to interact with.
  */
object DistributionEngine {
  var host = InetAddress.getLocalHost.getHostAddress
  // list of hosts for a given name
  // TODO: make private
  var registry: Map[String, List[String]] = Map().withDefaultValue(List())

  // fakes the existence of a server infrastructure
  // maps hostname to map of varName and value
  // TODO: make private
  var cloudStorage: Map[String, Map[String, Any]] = Map().withDefaultValue(Map())

  var localSignals: List[Signal[StateCRDT[_,_,_]]] = List()

  /**
    * Publishes a variable on this host to the other hosts.
    *
    * @param varName
    * @param crdt
    * @tparam A
    * @tparam B
    * @tparam C
    */
  def publish[A,B,C <: StateCRDT[A,B,C]](varName: String, localVar: Signal[StateCRDT[A, B, C]]) = {
    // LookupServer Registration:
    val crdt = localVar.now // retrieve current value of CRDT
    val hosts = LookupServer.register(varName, host) // register this instance
    registry = registry + (varName -> hosts) // update local registry

    // Fetch all other hosts:
    val mergedCrdt = registry(varName)
      .filter((hostname) => hostname != host)
      .foldLeft(crdt)((mergedCrdt, hostname) => // update local value by pulling all hosts
        mergedCrdt.merge(crdt.fromPayload(cloudStorage(hostname)(varName).asInstanceOf[B]))) // merge new value for this var with the the old one
    cloudStorage = cloudStorage +
      (host -> (cloudStorage(host) + (varName -> mergedCrdt.payload))) // update storage for this host
    set(varName, mergedCrdt) // update other hosts with new value
    localVar.changed += ((c:StateCRDT[_,B,C]) => set(varName, c))
  }

  /**
    * Updates the value of a given variable on all hosts.
    *
    * @param varName
    * @param value
    * @tparam C
    */
  def set[C <: StateCRDT[_,_,C]](varName: String, value: StateCRDT[_,_,C]) =
    registry(varName).filter((hostname) => hostname != host) // get all hosts differing from this one
      .foreach((hostname) => {
      cloudStorage = cloudStorage +
        (hostname -> (cloudStorage(hostname) +
          (varName -> value.payload))) // add new value to cloudStorage
      Thread sleep 2000
      println(hostname + " updated")
    })

  /**
    * Pull all hosts and return the new merged value for a given name
    */
  def update[A <: StateCRDT[_, _, A]](varName: String, localVal: A): A =
    registry(varName).foldLeft(localVal)((newValue, hostname) =>
      newValue.merge(cloudStorage(hostname)(varName).asInstanceOf[A]))
}

object LookupServer {
  // maps varName to list of sharing hosts
  private var registry: Map[String, List[String]] = Map().withDefaultValue(List())

  /**
    * Registers a new shared variable to the lookup server.
    * If the variable name is already known to the server,
    *
    * @param name
    * @tparam B
    * @return
    */
  def register[B](name: String, hostname: String): List[String] = {
    val listOfHosts: List[String] = registry(name)
    val newListOfHosts: List[String] = listOfHosts :+ hostname // append new id to list of ids
    registry = registry + (name -> newListOfHosts) // update registry
    newListOfHosts // return new list of ids
  }

  /**
    * Returns all hosts for this variable name
    *
    * @param name name of the shared variable
    * @return list of hosts sharing the variable
    *
    */
  def lookup(name: String): List[String] = registry(name)
}