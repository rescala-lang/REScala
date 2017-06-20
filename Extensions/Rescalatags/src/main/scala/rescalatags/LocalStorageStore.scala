package rescalatags

import rescala.restore.{ReStore, ReStoringStruct, ReStoringTurn}
import rescala.twoversion.TwoVersionEngine
import org.scalajs.dom
import org.scalajs.dom.raw.Storage

import scala.scalajs.js.JSON

class LocalStorageStore(domain: String = "") extends TwoVersionEngine[ReStoringStruct, ReStoringTurn] with ReStore {

  val storage: Storage = dom.window.localStorage

  var count = 0
  def nextName(): String = {
    count += 1
    domain + count
  }
  override def put(key: String, value: String): Unit = {
    println(s"store $key -> $value")
    println(s"${value.asInstanceOf[scala.scalajs.js.Any]}")
    println(s"${JSON.stringify(value.asInstanceOf[scala.scalajs.js.Any])}")
    storage.setItem(key, JSON.stringify(value.asInstanceOf[scala.scalajs.js.Any]))
  }
  override def get(key: String): Option[String] = {
    val res2 = Option({
      val res = storage.getItem(key)
      println(s"got $key -> $res")
      res
    }).map({ t =>
      val res3 = JSON.parse(t)
      println(s"parsed as $res3")
        res3.asInstanceOf[String]})
    println(s"parsed to $res2")
    res2
  }

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
  lazy override val toString: String = s"Engine(Restoring: $domain)"
}
