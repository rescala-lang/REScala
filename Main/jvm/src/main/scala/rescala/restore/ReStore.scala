package rescala.restore

import rescala.graph.{Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStructTypeImpl}
import rescala.propagation.Turn
import rescala.twoversion.{PlanImpl, TwoVersionPropagation}

case class Storing(current: Any, level: Int, incoming: Set[Reactive[Struct]])

class ReStoringTurn(engine: ReStoringEngine) extends LevelBasedPropagation[ReStoringStruct] {

  override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[ReStoringStruct]], hasState: Boolean): ReStoringStructType[P, ReStoringStruct] = {
    if (hasState) {
      val name = engine.nextName
      def store(storing: Storing) = {
        //println(s"updating $name to $storing")
        engine.values.put(name, storing)
      }
      engine.values.get(name) match {
        case None =>
          //println(s"new struct $name")
          new ReStoringStructType(store, initialValue, transient, initialIncoming)
        case Some(s@Storing(c, l, i)) =>
          //println(s"old struct $name $s")
          val res = new ReStoringStructType(store, c.asInstanceOf[P], transient, initialIncoming)
          res._level = l
          res
      }
    }
    else new ReStoringStructType(null, initialValue, transient, initialIncoming)
  }
  override def releasePhase(): Unit = ()
}

class ReStoringStructType[P, S <: Struct](storage: Storing => Unit, initialVal: P, transient: Boolean, initialIncoming: Set[Reactive[S]]) extends LevelStructTypeImpl[P, S](initialVal, transient, initialIncoming) {
  override def commit(implicit turn: TwoVersionPropagation[S]): Unit = {
    super.commit
    if (storage != null) storage(Storing(current, _level, _incoming.asInstanceOf[Set[Reactive[Struct]]]))
  }
}


trait ReStoringStruct extends LevelStruct {
  override type Type[P, S <: Struct] = ReStoringStructType[P, S]
}


class ReStoringEngine(domain: String = "", restoreFrom: Seq[(String, Storing)] = Nil) extends PlanImpl[ReStoringStruct, ReStoringTurn] {

  val values: scala.collection.mutable.HashMap[String, Storing] = scala.collection.mutable.HashMap(restoreFrom: _*)
  var count = 0
  def nextName(): String = {
    count += 1
    domain + count
  }
  def snapshot(): Map[String, Storing] = values.toMap

  override protected def makeTurn(priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
  lazy override val toString: String = s"Engine(Restoring: $domain)"
  override def plan[R](i: Reactive*)(f: ReStoringTurn => R): R = synchronized(super.plan(i: _*)(f))

}


