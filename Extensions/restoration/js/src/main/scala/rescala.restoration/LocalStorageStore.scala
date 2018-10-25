package rescala.restoration

import org.scalajs.dom
import org.scalajs.dom.Storage
import rescala.core.{REName, ReSerializable, Scheduler}
import rescala.debuggable.{ChromeDebuggerInterface, NodeID}
import rescala.interface.RescalaInterfaceRequireSerializer
import rescala.reactives.Source

import scala.reflect.ClassTag

class LocalStorageStore()
  extends ReStoreImpl
  with RescalaInterfaceRequireSerializer[ReStoringStruct] {

  val storage: Storage = dom.window.localStorage

  val snapshotcodec  : ReSerializable[Map[String, String]]              = ReCirce.recirce[Map[String, String]]
  val timetravelCodec: ReSerializable[Map[String, Map[String, String]]] = ReCirce.recirce[Map[String, Map[String, String]]]

  var currentSnapshot : Map[String, String]              = {
    Option(storage.getItem("snapshotStorage§currentSnapshot"))
    .flatMap(snapshotcodec.deserialize(_).toOption).getOrElse(Map())
  }
  var timetravelPoints: Map[String, Map[String, String]] = {
    Option(storage.getItem("snapshotStorage§timetravelPoints"))
    .flatMap(timetravelCodec.deserialize(_).toOption).getOrElse(Map())
  }

  var currentName: Int = Option(storage.getItem("snapshotStorage§currentName")).fold (0)(_.toInt)


  scala.scalajs.js.Object.keys(storage).filterNot(_.startsWith("snapshotStorage§")).foreach { k =>
    currentSnapshot += (k -> storage.getItem(k))
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
      case (name, value) => storage.setItem(name, value)
    }
    storage.setItem("snapshotStorage§timetravelPoints", timetravelCodec.serialize(timetravelPoints))
    dom.window.location.reload(false)
  }


  override def scheduler: Scheduler[ReStoringStruct] = this


  override def put(key: REName, value: String): Unit = {
//    println(s"store $key -> $value")
    currentSnapshot += (key.str -> value)
    storage.setItem(key.str, value)
  }
  override def get(key: REName): Option[String] = {
    val res = Option(storage.getItem(key.str))
//    println(s"parsed $key -> $res")
    res
  }

  override def schedulerName: String = s"LocalStorage"
  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this, ChromeDebuggerInterface)


  def registerSource[T: ClassTag](s: Source[ReStoringStruct, T], values: String*): Unit = {
    val ct = implicitly[ClassTag[T]]

    ChromeDebuggerInterface.sourceHint(NodeID(s.rename.str), ct.runtimeClass.getSimpleName, values)
  }

}
