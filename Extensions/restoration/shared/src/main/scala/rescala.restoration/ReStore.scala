package rescala.restoration

import rescala.core.Initializer.InitValues
import rescala.core.{Initializer, ReSerializable, ReSource, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStructTypeImpl}
import rescala.twoversion.{TwoVersionPropagation, TwoVersionScheduler}

import scala.collection.mutable

class ReStoringTurn(restore: ReStore) extends LevelBasedPropagation[ReStoringStruct] {

  override protected def makeDerivedStructState[P](valuePersistency: InitValues[P]): ReStoringStructType[P, ReStoringStruct] = {
    valuePersistency match {
      case is@Initializer.InitializedSignal(init) if is.serializable != rescala.core.ReSerializable.doNotSerialize =>
        if (is.serializable == rescala.core.ReSerializable.serializationUnavailable) throw new Exception(s"restore requires serializable reactive: $valuePersistency")
        val name = restore.nextName
        restore.get(name) match {
          case None =>
            //println(s"new struct $name")
            new ReStoringStructType[P, ReStoringStruct](restore, name, is.serializable, is)
          case Some(v) =>
            //println(s"old struct $name $s")
            val restoredValue = Initializer.InitializedSignal(is.serializable.deserialize(v).get)
            new ReStoringStructType[P, ReStoringStruct](restore, name, is.serializable, restoredValue)
        }
      case _ =>
        new ReStoringStructType(null, null, null, valuePersistency)
    }
  }

  override def dynamicDependencyInteraction(dependency: ReSource[ReStoringStruct]): Unit = ()
  override def releasePhase(): Unit = ()
  override def preparationPhase(initialWrites: Traversable[ReSource[ReStoringStruct]]): Unit = ()


}

class ReStoringStructType[P, S <: Struct](storage: ReStore, val name: String, serializable: ReSerializable[P], initialVal: InitValues[P])
  extends LevelStructTypeImpl[P, S](initialVal) {
  override def commit(turn: TwoVersionPropagation[S]): Unit = {
    super.commit(turn)
    if (storage != null) {
      storage.put(name, serializable.serialize(current))
    }
  }
}


trait ReStoringStruct extends LevelStruct {
  override type State[P, S <: Struct] = ReStoringStructType[P, S]
}

trait ReStore {
  def nextName(): String
  def put(key: String, value: String): Unit
  def get(key: String): Option[String]
}


class ReStoringScheduler(domain: String = "", restoreFrom: mutable.Map[String, String] = mutable.HashMap()) extends TwoVersionScheduler[ReStoringStruct, ReStoringTurn] with ReStore {

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
  lazy override val toString: String = s"Engine(Restoring: $domain)"
  override protected[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R): R =
    synchronized(super.executeTurn(initialWrites, admissionPhase))
}


