package rescala.restore

import rescala.graph.{Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStructTypeImpl}
import rescala.propagation.Turn
import rescala.twoversion.EngineImpl


object ReStore {
  val values: scala.collection.mutable.HashMap[String, Storing] = scala.collection.mutable.HashMap()
  var count = 0
  def crash() = count = 0
}

case class Key[P](name: String)
case class Storing(current: Any, level: Int, incoming: Set[Reactive[Struct]])

class ReStoringTurn() extends LevelBasedPropagation[ReStoringStruct] {

  override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[ReStoringStruct]], isFold: Boolean): ReStoringStructType[P, ReStoringStruct] = {
    if (isFold) {
      val name = ReStore.count.toString
      ReStore.count += 1
      ReStore.values.get(name) match {
        case None =>
          println(s"new struct $name")
          new ReStoringStructType(name, initialValue, transient, initialIncoming)
        case Some(s@Storing(c, l, i)) =>
          println(s"old struct $name $s")
          val res = new ReStoringStructType(name, c.asInstanceOf[P], transient, initialIncoming)
          res._level = l
          res
      }
    }
    else new ReStoringStructType("", initialValue, transient, initialIncoming)
  }
  override def releasePhase(): Unit = ()
}

class ReStoringStructType[P, S <: Struct](key: String, initialVal: P, transient: Boolean, initialIncoming: Set[Reactive[S]]) extends LevelStructTypeImpl[P, S](initialVal, transient, initialIncoming) {
  override def commit(implicit turn: Turn[S]): Unit = {
    super.commit
    if (key.nonEmpty) {
      println(s"updating $key to $current")
      ReStore.values.put(key, Storing(current, _level, _incoming.asInstanceOf[Set[Reactive[Struct]]]))
    }
  }
}


trait ReStoringStruct extends LevelStruct {
  override type Type[P, S <: Struct] = ReStoringStructType[P, S]
}

class ReStoringEngine extends EngineImpl[ReStoringStruct, ReStoringTurn]("Synchron", (_, _) => new ReStoringTurn()) {
  override def plan[R](i: Reactive*)(f: ReStoringTurn => R): R = synchronized(super.plan(i: _*)(f))

}


