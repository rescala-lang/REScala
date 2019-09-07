package rescala.extra.restoration

import org.scalajs.dom
import org.scalajs.dom.Storage
import rescala.core.{REName, Scheduler}
import rescala.extra.debuggable.DisableDebugging
import rescala.interface.RescalaInterfaceRequireSerializer

class LocalStorageStore()
  extends ReStoreImpl
  with RescalaInterfaceRequireSerializer[ReStoringStruct] {

  val storage: Storage = dom.window.localStorage

  override def scheduler: Scheduler[ReStoringStruct] = this

  override def put(key: REName, value: String): Unit = storage.setItem(key.str, value)
  override def get(key: REName): Option[String] = Option(storage.getItem(key.str))

  override def schedulerName: String = s"LocalStorage"
  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn =
    new ReStoringTurn(this, DisableDebugging)

}
