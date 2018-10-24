package rescala.restoration

import org.scalajs.dom
import org.scalajs.dom.Storage
import rescala.core.{REName, Scheduler}
import rescala.debuggable.ChromeDebuggerInterface
import rescala.interface.RescalaInterfaceRequireSerializer

class LocalStorageStore()
  extends ReStoreImpl
  with RescalaInterfaceRequireSerializer[ReStoringStruct] {

  var currentSnapshot: Map[REName, String] = Map()
  var timetravelPoints: Map[String, Map[REName, String]] = Map()
  var currentName: Int = 0

  val storage: Storage = dom.window.localStorage

  scala.scalajs.js.Object.keys(storage).foreach { k =>
    currentSnapshot += (REName(k) -> storage.getItem(k))
  }

  commitCurrentSnapshot()

  override def commitCurrentSnapshot(): String = {
    println(s"commiting $currentName $currentSnapshot")
    val id = currentName.toString
    timetravelPoints = timetravelPoints.updated(id, currentSnapshot)
    currentName += 1
    id
  }

  override def restoreSnap(snapId: String): Unit = {
    currentSnapshot = timetravelPoints.getOrElse(snapId, timetravelPoints.tail.head._2)
    println("========================")
    println(timetravelPoints)
    println(snapId)
    println(currentSnapshot)
    println("========================")
    storage.clear()
    currentSnapshot.foreach {
      case (name, value) => storage.setItem(name.str, value)
    }
    dom.window.location.reload(false)
  }


  override def scheduler: Scheduler[ReStoringStruct] = this


  override def put(key: REName, value: String): Unit = {
//    println(s"store $key -> $value")
    currentSnapshot += (key -> value)
    storage.setItem(key.str, value)
  }
  override def get(key: REName): Option[String] = {
    val res = Option(storage.getItem(key.str))
//    println(s"parsed $key -> $res")
    res
  }

  override def schedulerName: String = s"LocalStorage"
  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this, ChromeDebuggerInterface)



}
