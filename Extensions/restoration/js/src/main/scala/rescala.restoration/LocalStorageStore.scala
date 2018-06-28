package rescala.restoration

import org.scalajs.dom
import org.scalajs.dom.Storage
import rescala.core.Scheduler
import rescala.interface.RescalaInterfaceRequireSerializer

class LocalStorageStore(override val domain: String = "")
  extends ReStoreImpl
  with RescalaInterfaceRequireSerializer[ReStoringStruct] {



  override def scheduler: Scheduler[ReStoringStruct] = this

  val storage: Storage = dom.window.localStorage

  override def put(key: String, value: String): Unit = {
    println(s"store $key -> $value")
    storage.setItem(key, value)
  }
  override def get(key: String): Option[String] = {
    val res = Option(storage.getItem(key))
    println(s"parsed $key -> $res")
    res
  }

  override def schedulerName: String = s"Restoring[$domain]"
  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
}
