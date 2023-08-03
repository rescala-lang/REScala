package kofre.datatypes.contextual

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.{Epoche, GrowOnlyList, LastWriterWins}
import kofre.dotted.{DotFun, Dotted, HasDots}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}

import scala.math.Ordering.Implicits.infixOrderingOps

/** An RGA (Replicated Growable Array) is a Delta CRDT modeling a list.
  *
  * When two values are concurrently inserted at an index i, the value of the insert operation with the later timestamp
  * will be at index i while the earlier inserted value will be pushed to index i+1. When an element is subject to two
  * concurrent updates, the later update overwrites the earlier update. If an element is concurrently updated and deleted,
  * the element will simply be deleted, ignoring the update.
  *
  * Note that RGAs are implemented as linked lists, thus the time needed to execute operations toward the end of the list
  * will scale linearly with the size of the list.
  *
  * To correctly handle concurrent remote inserts next to elements that were deleted locally, the RGA implementation internally
  * keeps deleted elements as hidden tombstones in the list. Since many tombstones will slow down the operations on this
  * data structure, purgeTombstones should be executed periodically to remove tombstones from the list. Note however that
  * this invalidates any concurrent insert operations. Ideally, purgeTombstones should only be called in downtime periods
  * and only by privileged replicas.
  *
  * This implementation was modeled after the RGA proposed by Roh et al. in "Replicated abstract data types: Building blocks
  * for collaborative applications", see [[https://www.sciencedirect.com/science/article/pii/S0743731510002716?casa_token=lQaLin7aEvcAAAAA:Esc3h3WvkFHUcvhalTPPvV5HbJge91D4-2jyKiSlz8GBDjx31l4xvfH8DIstmQ973PVi46ckXHg here]]
  */
case class ReplicatedList[E](order: Epoche[GrowOnlyList[Dot]], meta: DotFun[ReplicatedList.Node[E]])
object ReplicatedList {

  def empty[E]: ReplicatedList[E] = ReplicatedList(Epoche.empty, DotFun.empty)

  given lattice[E]: Lattice[ReplicatedList[E]] =
    val derived = Lattice.derived[ReplicatedList[E]]
    new Lattice[ReplicatedList[E]] {
      export derived.merge

      override def decompose(a: ReplicatedList[E]): Iterable[ReplicatedList[E]] =
        a.meta.decomposed.flatMap: (df: DotFun[ReplicatedList.Node[E]]) =>
          val dots = df.dots
          a.order.value.inner.find: (_, v) =>
            dots.contains(v.value.payload)
          .map: kv =>
            ReplicatedList(a.order.copy(value = GrowOnlyList(Map(kv))), df)

    }
  given hasDots[E]: HasDots[ReplicatedList[E]] with {
    extension (dotted: ReplicatedList[E])
      def dots: Dots = dotted.meta.dots union Dots.from(dotted.order.value.growOnlyList.toList)
      def removeDots(dots: Dots): Option[ReplicatedList[E]] =
        val nmeta = dotted.meta.repr.map: (k, v) =>
          if dots.contains(k)
          then (k, Node.Dead)
          else (k, v)
        .toMap

        if nmeta.isEmpty then None
        else Some(dotted.copy(meta = DotFun(nmeta)))
  }

  given bottom[E]: Bottom[ReplicatedList[E]] = new:
    override def empty: ReplicatedList[E] = ReplicatedList.empty


  enum Node[+A]:
    case Alive(v: LastWriterWins[A])
    case Dead
  import Node.{Alive, Dead}

  object Node {

    /** Lattice y order: Dead > Alive */
    given lattice[A]: Lattice[Node[A]] with {
      override def lteq(left: Node[A], right: Node[A]): Boolean = (left, right) match
        case (Dead, _)              => false
        case (_, Dead)              => true
        case (Alive(lv), Alive(rv)) => rv.timestamp > lv.timestamp

      override def merge(left: Node[A], right: Node[A]): Node[A] = (left, right) match {
        case (Alive(lv), Alive(rv)) => Alive(Lattice[LastWriterWins[A]].merge(lv, rv))
        case _                      => Dead
      }
    }
  }

  private class DeltaStateFactory[E] {

    def make(
        epoche: Epoche[GrowOnlyList[Dot]] = empty._1,
        df: DotFun[Node[E]] = DotFun.empty,
        cc: Dots = Dots.empty
    ): Dotted[ReplicatedList[E]] = Dotted(ReplicatedList(epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

  extension [C, E](container: C)
    def replicatedList: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C)
      extends OpsSyntaxHelper[C, ReplicatedList[E]](container) {

    def read(using PermQuery)(i: Int): Option[E] = {
      val ReplicatedList(fw, df) = current
      fw.value.toLazyList.map(df.repr).collect {
        case Alive(tv) => tv.payload
      }.lift(i)
    }

    def size(using PermQuery): Int = {
      current.meta.repr.values.count {
        case Dead     => false
        case Alive(_) => true
      }
    }

    def toList(using PermQuery): List[E] = {
      val ReplicatedList(fw, df) = current
      fw.value.growOnlyList.toList.map(df.repr).collect {
        case Alive(tv) => tv.payload
      }
    }

    def sequence(using PermQuery): Long = {
      val ReplicatedList(fw, _) = current
      fw.counter
    }

    private def findInsertIndex(state: ReplicatedList[E], n: Int): Option[Int] = state match {
      case ReplicatedList(fw, df) =>
        fw.value.toLazyList.zip(LazyList.from(1)).filter {
          case (dot, _) => df.repr(dot) match {
              case Alive(_) => true
              case Dead     => false
            }
        }.map(_._2).prepended(0).lift(n)
    }

    def insert(using ReplicaId, PermCausalMutate)(i: Int, e: E): C = {
      val ReplicatedList(fw, df) = current
      val nextDot                = context.nextDot(replicaId)

      findInsertIndex(current, i) match {
        case None => Dotted(ReplicatedList.empty[E])
        case Some(glistInsertIndex) =>
          val glistDelta = fw.map { gl =>
            gl.insertGL(glistInsertIndex, nextDot)
          }
          val dfDelta = DotFun.single(nextDot, Alive(LastWriterWins.now(e)))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = Dots.single(nextDot)
          )
      }
    }.mutator

    def insertAll(using ReplicaId, PermCausalMutate)(i: Int, elems: Iterable[E]): C = {
      val ReplicatedList(fw, df) = current
      val nextDot                = context.nextDot(replicaId)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c, r + 1)
      }

      findInsertIndex(current, i) match {
        case None => Dotted(ReplicatedList.empty)
        case Some(glistInsertIndex) =>
          val glistDelta =
            fw.map { gl =>
              gl.insertAllGL(glistInsertIndex, nextDots)
            }
          val dfDelta = DotFun.empty[Node[E]].repr ++ (nextDots zip elems.map(e => Alive(LastWriterWins.now(e))))

          deltaState[E].make(
            epoche = glistDelta,
            df = DotFun(dfDelta),
            cc = Dots.from(nextDots.toSet)
          )
      }
    }.mutator

    private def updateRGANode(state: ReplicatedList[E], i: Int, newNode: Option[E]): Dotted[ReplicatedList[E]] = {
      val ReplicatedList(fw, df) = state
      fw.value.toLazyList.lift(i) match {
        case None => Dotted(ReplicatedList.empty)

        case Some(d) =>
          df.repr(d) match
            case Dead =>  Dotted(ReplicatedList.empty)
            case Alive(current) =>
              newNode match
                case None => deltaState[E].make(df = DotFun.single(d, Dead), cc = Dots.single(d))
                case Some(value) => deltaState[E].make(df = DotFun.single(d, Alive(current.write(value))), cc = Dots.single(d))
      }
    }

    def update(using ReplicaId, PermCausalMutate)(i: Int, e: E): C =
      updateRGANode(current, i, Some(e)).mutator

    def delete(using ReplicaId, PermCausalMutate)(i: Int): C = updateRGANode(current, i, None).mutator

    private def updateRGANodeBy(
        state: ReplicatedList[E],
        cond: E => Boolean,
        newNode: Node[E]
    ): Dotted[ReplicatedList[E]] = {
      val ReplicatedList(_, df) = state
      val toUpdate = df.repr.toList.collect {
        case (d, Alive(tv)) if cond(tv.payload) => d
      }

      deltaState[E].make(df = DotFun(toUpdate.iterator.map(_ -> newNode).toMap))
    }

    def updateBy(using ReplicaId, PermCausalMutate)(cond: E => Boolean, e: E): C =
      updateRGANodeBy(current, cond, Alive(LastWriterWins.now(e))).mutator

    def deleteBy(using ReplicaId, PermCausalMutate)(cond: E => Boolean): C =
      updateRGANodeBy(current, cond, Dead).mutator

    def purgeTombstones(using ReplicaId, PermCausalMutate)(): C = {
      val ReplicatedList(epoche, df) = current
      val toRemove = df.repr.collect {
        case (dot, Dead) => dot
      }.toSet

      val golistPurged = epoche.value.without(toRemove)

      deltaState[E].make(
        epoche = epoche.epocheWrite(golistPurged),
        cc = Dots.from(toRemove)
      ).mutator
    }

    def clear(using PermCausalMutate)(): C = {
      deltaState[E].make(
        cc = context
      ).mutator
    }

    def prepend(using ReplicaId, PermCausalMutate)(e: E): C = insert(0, e)

    def append(using ReplicaId, PermCausalMutate)(e: E): C = insert(size, e)

    def prependAll(using ReplicaId, PermCausalMutate)(elems: Iterable[E]): C = insertAll(0, elems)

    def appendAll(using ReplicaId, PermCausalMutate)(elems: Iterable[E]): C = insertAll(size, elems)

  }
}
