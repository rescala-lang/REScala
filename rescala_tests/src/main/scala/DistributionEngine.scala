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

  var localVars: Map[String, Map[String, Any]] = Map().withDefaultValue(Map())

  /**
    * Publishes a variable on this host to the other hosts.
    *
    * @param varName
    * @param crdt
    * @tparam A
    * @tparam B
    * @tparam C
    */
  def publish(varName: String, localVar: Var[_ <: StateCRDT]) = {
    // LookupServer Registration:
    val crdt = localVar.now // retrieve current value of CRDT
    val hosts = LookupServer.register(varName, host) // register this instance
    registry = registry + (varName -> hosts) // update local registry

    //set(varName, mergedCrdt) // update other hosts with new value

    println("Adding listener for " + localVar)

    localVar.changed += ((c) => {
      println("localVar " + c.asInstanceOf[CIncOnlyCounter].id + " changed!")
      set(varName, c)
    })

    // Fetch all other hosts:
    val mergedCrdt = registry(varName)
      .filter((hostname) => hostname != host)
      .foldLeft(crdt)((mergedCrdt, hostname) => // update local value by pulling all hosts
        mergedCrdt.merge(mergedCrdt.fromPayload(cloudStorage(hostname)(varName).asInstanceOf[mergedCrdt.payloadType]))) // merge new value for this var with the the old one

    localVar.asInstanceOf[Var[StateCRDT]].set(mergedCrdt) // update value with mergedCrdt
    cloudStorage = cloudStorage +
      (host -> (cloudStorage(host) + (varName -> mergedCrdt.payload))) // update storage for this host

    localVars = localVars + (host -> (localVars(host) + (varName -> localVar))) // add the var to this host
  }

  /**
    * Updates the value of a given variable on all hosts.
    *
    * @param varName
    * @param value
    * @tparam C
    */
  def set(varName: String, value: StateCRDT) =
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

  /**
    * Pull all hosts and return the new merged value for a given name
    */
  def update(varName: String, localVal: StateCRDT) =
    registry(varName).foldLeft(localVal)((newValue, hostname) =>
      newValue.merge(newValue.fromPayload(cloudStorage(hostname)(varName).asInstanceOf[newValue.payloadType])).asInstanceOf[StateCRDT])
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