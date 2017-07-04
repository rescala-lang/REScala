import java.net.InetAddress

/**
  * Handles distribution on the client side and provides a frontend for the programmer to interact with.
  */
object DistributionEngine {
  var host = InetAddress.getLocalHost.getHostAddress
  // list of hosts for a given name
  private var registry: Map[String, List[String]] = Map().withDefaultValue(List())

  // fakes the existence of a server infrastructure
  // maps hostname to map of varName and value
  private var cloudStorage: Map[String, Map[String, Any]] = Map().withDefaultValue(Map().withDefaultValue(None))

  /**
    * Publishes a variable on this host to the other hosts.
    *
    * @param varName
    * @param crdt
    * @tparam A
    * @tparam B
    * @tparam C
    */
  def publish[A, B, C <: StateCRDT[A, B, C]](varName: String, crdt: StateCRDT[A, B, C]) = {
    val (id, hosts) = LookupServer.register(varName) // register this instance
    registry = registry + (varName -> hosts) // update local registry
    val mergedCrdt = registry(varName).foldLeft(crdt)((mergedCrdt, hostname) => // update local value by pulling all hosts
      mergedCrdt.merge(crdt.fromPayload(cloudStorage(hostname)(varName).asInstanceOf[B]))) // merge new value for this var with the the old one
    set(varName, mergedCrdt) // update other hosts with new value
    id // return the assigned id
  }

  /**
    * Updates the value of a given variable on all hosts.
    *
    * @param varName
    * @param value
    * @tparam A
    * @tparam B
    * @tparam C
    */
  def set[A, B, C <: StateCRDT[A, B, C]](varName: String, value: StateCRDT[A, B, C]) =
    cloudStorage = cloudStorage.mapValues((hostValues) => hostValues + (varName -> value.payload))

  /**
    * Pull all hosts and return the new merged value for a given name
    */
  def update[A, B, C <: StateCRDT[A, B, C]](varName: String): StateCRDT[A, B, C] =
    registry(varName).foldLeft(new C)((newValue, hostname) =>
      newValue.merge(cloudStorage(hostname)(varName).asInstanceOf[C]))
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
  def register[B](name: String): (String, List[String]) = {
    val listOfHosts: List[String] = registry(name)
    val newHost: String = name + "-" + listOfHosts.length.toString
    val newListOfHosts: List[String] = listOfHosts :+ newHost // append new id to list of ids
    registry = registry + (name -> newListOfHosts) // update registry
    (newHost, listOfHosts) // return new list of ids
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