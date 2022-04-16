package kofre.decompose.interfaces
import kofre.decompose.*
import kofre.syntax.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.DotFun

object LWWRegisterInterface {
  type State[A] = MVRegisterInterface.State[TimedVal[A]]

  trait LWWRegisterCompanion {
    type State[A]    = LWWRegisterInterface.State[A]
    type Embedded[A] = DotFun[TimedVal[A]]
  }

  def read[A]: DeltaQuery[State[A], Option[A]] =
    MVRegisterInterface.read[TimedVal[A]].andThen(s => s.reduceOption(UIJDLattice[TimedVal[A]].merge).map(_.value))

  def write[A](v: A): DeltaMutator[State[A]] =
    (replicaID, state) => MVRegisterInterface.write(TimedVal(v, replicaID)).apply(replicaID, state)

  def map[A](f: A => A): DeltaMutator[State[A]] = (replicaID, state) =>
    read[A].apply(state).map(f) match {
      case None    => UIJDLattice[State[A]].bottom
      case Some(v) => write(v).apply(replicaID, state)
    }

  def clear[A](): DeltaMutator[State[A]] = MVRegisterInterface.clear()
}

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
abstract class LWWRegisterInterface[A, Wrapper]
    extends CRDTInterface[LWWRegisterInterface.State[A], Wrapper] {
  def read: Option[A] = query(LWWRegisterInterface.read)

  def write(v: A): Wrapper = mutate(LWWRegisterInterface.write(v))

  def map(f: A => A): Wrapper = mutate(LWWRegisterInterface.map(f))

  def clear(): Wrapper = mutate(LWWRegisterInterface.clear())
}
