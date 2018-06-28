package rescala.restoration

import rescala.core.Initializer.InitValues
import rescala.core.{Initializer, ReSerializable, ReSource, Scheduler, Struct}
import rescala.interface.RescalaInterface
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStateImpl}
import rescala.twoversion.TwoVersionScheduler

import scala.collection.mutable

object RestoringInterface {
  def apply(domain: String = "", restoreFrom: mutable.Map[String, String] = mutable.HashMap()): RestoringInterface =
    new RestoringInterface(domain, restoreFrom)
}


class ReStoringTurn(restore: ReStore) extends LevelBasedPropagation[ReStoringStruct] {

  override protected def makeDerivedStructState[P](valuePersistency: InitValues[P]): ReStoringState[P, ReStoringStruct] = {
    valuePersistency match {
      case is@Initializer.InitializedSignal(init) if is.serializable != rescala.core.ReSerializable.doNotSerialize =>
        if (is.serializable == rescala.core.ReSerializable.serializationUnavailable) throw new Exception(s"restore requires serializable reactive: $valuePersistency")
        val name = restore.nextName
        restore.get(name) match {
          case None =>
            //println(s"new struct $name")
            new ReStoringState[P, ReStoringStruct](restore, name, is.serializable, is)
          case Some(v) =>
            //println(s"old struct $name $s")
            val restoredValue = Initializer.InitializedSignal(is.serializable.deserialize(v).get)(is.serializable)
            new ReStoringState[P, ReStoringStruct](restore, name, is.serializable, restoredValue)
        }
      case _ =>
        new ReStoringState(null, null, null, valuePersistency)
    }
  }

  override def dynamicDependencyInteraction(dependency: ReSource[ReStoringStruct]): Unit = ()
  override def releasePhase(): Unit = ()
  override def preparationPhase(initialWrites: Set[ReSource[ReStoringStruct]]): Unit = ()


}

class ReStoringState[P, S <: Struct](storage: ReStore,
                                     val name: String,
                                     serializable: ReSerializable[P],
                                     initialVal: InitValues[P])
  extends LevelStateImpl[P, S](initialVal) {
  override def commit(): Unit = {
    super.commit()
    if (storage != null) {
      storage.put(name, serializable.serialize(current))
    }
  }
}


trait ReStoringStruct extends LevelStruct {
  override type State[P, S <: Struct] = ReStoringState[P, S]
}

trait ReStore {
  def nextName(): String
  def put(key: String, value: String): Unit
  def get(key: String): Option[String]
}


class RestoringInterface(domain: String, restoreFrom: mutable.Map[String, String])
  extends TwoVersionScheduler[ReStoringStruct, ReStoringTurn] with ReStore with RescalaInterface[ReStoringStruct] {

  override def scheduler: Scheduler[ReStoringStruct] = this

  def values: mutable.Map[String, String] = restoreFrom
  var count = 0
  def nextName(): String = {
    count += 1
    domain + count
  }
  override def put(key: String, value: String): Unit = values.put(key, value)
  override def get(key: String): Option[String] = values.get(key)
  def snapshot(): mutable.Map[String, String] = values

  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
  override def schedulerName : String = s"Restoring[$domain]"
  override def executeTurn[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R =
    synchronized(super.executeTurn(initialWrites, admissionPhase))
}


