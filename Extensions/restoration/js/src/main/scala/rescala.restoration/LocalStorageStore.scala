package rescala.restoration

import org.scalajs.dom
import org.scalajs.dom.Storage
import rescala.core.{REName, Scheduler}
import rescala.interface.RescalaInterfaceRequireSerializer

class LocalStorageStore()
  extends ReStoreImpl
  with RescalaInterfaceRequireSerializer[ReStoringStruct] {



  override def scheduler: Scheduler[ReStoringStruct] = this

  val storage: Storage = dom.window.localStorage

  override def put(key: REName, value: String): Unit = {
    println(s"store $key -> $value")
    storage.setItem(key.name, value)
  }
  override def get(key: REName): Option[String] = {
    val res = Option(storage.getItem(key.name))
    println(s"parsed $key -> $res")
    res
  }

  override def schedulerName: String = s"LocalStorage"
  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
}
