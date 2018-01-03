package rescala.restore

import rescala.core.{ReSerializable, ReSource, Struct, ValuePersistency}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStructTypeImpl}
import rescala.twoversion.{TwoVersionEngine, TwoVersionPropagation}

import scala.collection.mutable

class ReStoringTurn(restore: ReStore) extends LevelBasedPropagation[ReStoringStruct] {

  override protected def makeDerivedStructState[P](valuePersistency: ValuePersistency[P]): ReStoringStructType[P, ReStoringStruct] = {
    valuePersistency match {
      case is@ValuePersistency.InitializedSignal(init) if is.serializable != rescala.core.ReSerializable.doNotSerialize =>
        if (is.serializable == rescala.core.ReSerializable.serializationUnavailable) throw new Exception(s"restore requires serializable reactive: $valuePersistency")
        val name = restore.nextName
        restore.get(name) match {
          case None =>
            //println(s"new struct $name")
            new ReStoringStructType[P, ReStoringStruct](restore, name, is.serializable, init, false)
          case Some(v) =>
            //println(s"old struct $name $s")
            new ReStoringStructType[P, ReStoringStruct](restore, name, is.serializable, is.serializable.deserialize(v).get, false)
        }
      case _ =>
        new ReStoringStructType(null, null, null, valuePersistency.initialValue, valuePersistency.isTransient)
    }
  }

  override def dynamicDependencyInteraction(dependency: ReSource[ReStoringStruct]): Unit = ()
  override def releasePhase(): Unit = ()
  override def preparationPhase(initialWrites: Traversable[ReSource[ReStoringStruct]]): Unit = ()


}

class ReStoringStructType[P, S <: Struct](storage: ReStore, val name: String, serializable: ReSerializable[P], initialVal: P, transient: Boolean) extends LevelStructTypeImpl[P, S](initialVal, transient) {
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


class ReStoringEngine(domain: String = "", restoreFrom: mutable.Map[String, String] = mutable.HashMap()) extends TwoVersionEngine[ReStoringStruct, ReStoringTurn] with ReStore {

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


