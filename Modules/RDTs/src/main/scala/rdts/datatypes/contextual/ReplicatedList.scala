package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice}
import rdts.datatypes.contextual.ReplicatedList.deltaState
import rdts.datatypes.{Epoch, GrowOnlyList, LastWriterWins}
import rdts.dotted.HasDots.mapInstance
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{LocalUid, OpsSyntaxHelper, PermQuery}
import rdts.time.{Dot, Dots}

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
  * However, since then the implementation was changed significantly, thus it may be a different or even a novel strategy by now.
  */
case class ReplicatedList[E](order: Epoch[GrowOnlyList[Dot]], meta: Map[Dot, LastWriterWins[E]]) {

  private def current = this
  type C = Dotted[ReplicatedList[E]]

  def read(i: Int): Option[E] = {
    val ReplicatedList(fw, df) = this
    fw.value.toLazyList.flatMap(df.get).map(_.payload).lift(i)
  }

  def size: Int = meta.size

  def toList: List[E] = {
    val ReplicatedList(fw, df) = this
    fw.value.toList.flatMap(df.get).map(_.payload)
  }

  def sequence: Long = {
    val ReplicatedList(fw, _) = this
    fw.counter
  }

  private def findInsertIndex(state: ReplicatedList[E], n: Int): Option[Int] = state match {
    case ReplicatedList(fw, df) =>
      fw.value.toLazyList.zip(LazyList.from(1)).filter {
        case (dot, _) => df.contains(dot)
      }.map(_._2).prepended(0).lift(n)
  }

  def insert(using LocalUid)(i: Int, e: E)(using context: Dots): C = {
    val ReplicatedList(order, entries) = current
    val nextDot                        = context.nextDot(LocalUid.replicaId)

    findInsertIndex(current, i) match {
      case None => Dotted(ReplicatedList.empty[E])
      case Some(glistInsertIndex) =>
        val glistDelta = order.map { gl =>
          gl.insertGL(glistInsertIndex, nextDot)
        }
        val dfDelta = Map(nextDot -> LastWriterWins.now(e))

        deltaState[E].make(
          epoche = glistDelta,
          df = dfDelta,
          cc = Dots.single(nextDot)
        )
    }
  }

  def insertAll(using LocalUid)(i: Int, elems: Iterable[E])(using context: Dots): C = {
    val ReplicatedList(fw, df) = current
    val nextDot                = context.nextDot(LocalUid.replicaId)

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
        val dfDelta = Map.empty[Dot, LastWriterWins[E]] ++ (nextDots zip elems.map(e => LastWriterWins.now(e)))

        deltaState[E].make(
          epoche = glistDelta,
          df = dfDelta,
          cc = Dots.from(nextDots.toSet)
        )
    }
  }

  private def updateRGANode(state: ReplicatedList[E], i: Int, newNode: Option[E]): Dotted[ReplicatedList[E]] = {
    val ReplicatedList(fw, df) = state
    fw.value.toLazyList.lift(i) match {
      case None => Dotted(ReplicatedList.empty)
      case Some(d) =>
        df.get(d) match
          case None => Dotted(ReplicatedList.empty)
          case Some(current) =>
            newNode match
              case None => deltaState[E].make(cc = Dots.single(d))
              case Some(value) =>
                deltaState[E].make(df = Map(d -> current.write(value)), cc = Dots.single(d))
    }
  }

  def update(using LocalUid)(i: Int, e: E): C =
    updateRGANode(current, i, Some(e))

  def delete(using LocalUid)(i: Int): C = updateRGANode(current, i, None)

  private def updateRGANodeBy(
      state: ReplicatedList[E],
      cond: E => Boolean,
      transform: LastWriterWins[E] => Option[LastWriterWins[E]]
  ): Dotted[ReplicatedList[E]] = {
    val touched: Iterable[Dot] = state.meta.flatMap: (k, v) =>
      Option.when(cond(v.payload))(k)

    val updates =
      touched.flatMap: dot =>
        val value = state.meta(dot)
        transform(value).map(nv => dot -> nv)
      .toMap

    deltaState[E].make(df = updates, cc = Dots.from(touched))
  }

  def updateBy(using LocalUid)(cond: E => Boolean, e: E): C =
    updateRGANodeBy(current, cond, old => Some(old.write(e)))

  def deleteBy(using LocalUid)(cond: E => Boolean): C =
    updateRGANodeBy(current, cond, _ => None)

  def purgeTombstones(using LocalUid)(): C = {
    val ReplicatedList(epoche, df) = current

    val known: List[Dot] = epoche.value.toList

    val contained = df.dots

    val removed = known.filter(dot => !contained.contains(dot))

    val golistPurged = epoche.value.without(removed.toSet)

    deltaState[E].make(
      epoche = epoche.epocheWrite(golistPurged),
      cc = Dots.from(removed)
    )
  }

  def clear()(using context: Dots): C = {
    deltaState[E].make(
      cc = context
    )
  }

  def prepend(using LocalUid)(e: E)(using context: Dots): C = insert(0, e)

  def append(using LocalUid)(e: E)(using context: Dots): C = insert(size, e)

  def prependAll(using LocalUid)(elems: Iterable[E])(using context: Dots): C = insertAll(0, elems)

  def appendAll(using LocalUid)(elems: Iterable[E])(using context: Dots): C = insertAll(size, elems)

}
object ReplicatedList {

  def empty[E]: ReplicatedList[E] = ReplicatedList(Epoch.empty, Map.empty)

  given lattice[E]: Lattice[ReplicatedList[E]] = Lattice.derived
  given hasDots[E]: HasDots[ReplicatedList[E]] with {
    extension (dotted: ReplicatedList[E])
      def dots: Dots = dotted.meta.dots
      def removeDots(dots: Dots): Option[ReplicatedList[E]] =
        val nmeta = dotted.meta.filter((k, _) => !dots.contains(k))

        if nmeta.isEmpty && dotted.order.isEmpty then None
        else Some(dotted.copy(meta = nmeta))
  }

  given bottom[E]: Bottom[ReplicatedList[E]] = new:
    override def empty: ReplicatedList[E] = ReplicatedList.empty

  private class DeltaStateFactory[E] {

    def make(
        epoche: Epoch[GrowOnlyList[Dot]] = empty._1,
        df: Map[Dot, LastWriterWins[E]] = Map.empty,
        cc: Dots = Dots.empty
    ): Dotted[ReplicatedList[E]] = Dotted(ReplicatedList(epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
