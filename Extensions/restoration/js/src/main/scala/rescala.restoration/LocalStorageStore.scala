package rescala.restoration

import org.scalajs.dom
import org.scalajs.dom.Storage
import rescala.twoversion.TwoVersionScheduler

import scala.collection.mutable
import scala.util.Random

class LocalStorageStore(domain: String = "") extends TwoVersionScheduler[ReStoringStruct, ReStoringTurn] with ReStore {

  val storage: Storage = dom.window.localStorage

  var count = 0
  val nextNames: mutable.Queue[String] = mutable.Queue.empty

  def nextName(): String = {
    if (nextNames.nonEmpty) {
      nextNames.dequeue()
    }
    else
    if (_currentTurn.value.isDefined) {
      domain + Random.nextLong()
    }
    else {
      count += 1
      domain + count
    }
  }


  def addNextNames(n: String*) = nextNames.enqueue(n: _*)

  override def put(key: String, value: String): Unit = {
    println(s"store $key -> $value")
    storage.setItem(key, value)
  }
  override def get(key: String): Option[String] = {
    val res = Option(storage.getItem(key))
    println(s"parsed $key -> $res")
    res
  }

  def getName(r: ReSource) = r.state.name

  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
  lazy override val toString: String = s"Engine(Restoring: $domain)"
}
