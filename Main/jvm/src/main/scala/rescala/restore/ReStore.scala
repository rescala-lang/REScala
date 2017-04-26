package rescala.restore

import rescala.graph.Pulse.NoChange
import rescala.graph.{Change, Pulse, Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct, LevelStructTypeImpl}
import rescala.twoversion.{TwoVersionEngine, TwoVersionPropagation}

case class Storing(current: Any, level: Int, incoming: Set[Reactive[Struct]])

class ReStoringTurn(engine: ReStoringEngine) extends LevelBasedPropagation[ReStoringStruct] {

  override protected def makeStructState[P](valueOrTransient: Option[Change[P]], hasAccumulatingState: Boolean): ReStoringStructType[Pulse[P], ReStoringStruct] = {
    if (hasAccumulatingState) {
      val name = engine.nextName
      def store(storing: Storing) = {
        //println(s"updating $name to $storing")
        engine.values.put(name, storing)
      }
      engine.values.get(name) match {
        case None =>
          //println(s"new struct $name")
          new ReStoringStructType(store, valueOrTransient.getOrElse(NoChange), valueOrTransient.isEmpty)
        case Some(s@Storing(c, l, i)) =>
          //println(s"old struct $name $s")
          val res = new ReStoringStructType[Pulse[P], ReStoringStruct](store, c.asInstanceOf[Pulse[P]], valueOrTransient.isEmpty)
          res._level = l
          res
      }
    }
    else new ReStoringStructType(null, valueOrTransient.getOrElse(NoChange), valueOrTransient.isEmpty)
  }
  override def releasePhase(): Unit = ()
}

class ReStoringStructType[P, S <: Struct](storage: Storing => Unit, initialVal: P, transient: Boolean) extends LevelStructTypeImpl[P, S](initialVal, transient) {
  override def commit(implicit turn: TwoVersionPropagation[S]): Unit = {
    super.commit
    if (storage != null) storage(Storing(current, _level, _incoming.asInstanceOf[Set[Reactive[Struct]]]))
  }
}


trait ReStoringStruct extends LevelStruct {
  override type State[P, S <: Struct] = ReStoringStructType[P, S]
}


class ReStoringEngine(domain: String = "", restoreFrom: Seq[(String, Storing)] = Nil) extends TwoVersionEngine[ReStoringStruct, ReStoringTurn] {

  val values: scala.collection.mutable.HashMap[String, Storing] = scala.collection.mutable.HashMap(restoreFrom: _*)
  var count = 0
  def nextName(): String = {
    count += 1
    domain + count
  }
  def snapshot(): Map[String, Storing] = values.toMap

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[ReStoringTurn]): ReStoringTurn = new ReStoringTurn(this)
  lazy override val toString: String = s"Engine(Restoring: $domain)"
  override private[rescala] def executeTurn[R](initialWrites: Traversable[Reactive], admissionPhase: ReStoringTurn => R): R = synchronized(super.executeTurn(initialWrites, admissionPhase))
}


