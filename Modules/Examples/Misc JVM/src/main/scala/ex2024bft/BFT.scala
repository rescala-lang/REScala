package ex2024bft

import rdts.base.Lattice.mapLattice
import rdts.base.{Bottom, Lattice}

import java.security.MessageDigest
import java.util
import scala.collection.mutable

trait Byteable[T] {

  def toBytes(obj: T): Array[Byte]

}

object Byteable {

  def toStringBased[T]: Byteable[T] = (obj: T) => obj.toString.getBytes

}

case class Hash(content: Array[Byte]) {
  override def toString: String = s"[#${content.mkString("")}]"

  override def canEqual(that: Any): Boolean = that.getClass == getClass

  override def equals(obj: Any): Boolean = canEqual(obj) && util.Arrays.equals(obj.asInstanceOf[Hash].content, content)
}

case class BFTDelta[V](value: V, predecessors: Set[Hash], hash: Hash) {

  def hashCorrect(using Byteable[V]): Boolean = {
    BFT.hash(value, predecessors) == hash
  }

}

object BFTDelta {

  def apply[V](value: V, predecessors: Set[Hash])(using Byteable[V]): BFTDelta[V] =
    BFTDelta(value, predecessors, BFT.hash(value, predecessors))

}

case class BFT[V](deltas: Set[BFTDelta[V]]) {

  def value(using b: Bottom[V], lat: Lattice[V]): V = {
    val graph = reverseGraph()

    if !graph.contains(None) then return b.empty

    val worklist = mutable.Queue[BFTDelta[V]](graph(None).toList*)

    val connected = mutable.Set.empty[BFTDelta[V]]
    val connectedHashes = mutable.Set.empty[Hash]

    while worklist.nonEmpty do {
      val elem = worklist.dequeue()
      connected.add(elem)
      connectedHashes.add(elem.hash)
      worklist.enqueueAll(graph.getOrElse(Some(elem.hash), Set.empty[BFTDelta[V]]).filter(delta => delta.predecessors.subsetOf(connectedHashes)))
    }

    connected.map(_.value).foldLeft(b.empty)((l, r) => l.merge(r))
  }

  /** Generates a BFT delta containing an update with a value delta. This assumes, that the RDT that's wrapped in the
    * BFT generates deltas.
    *
    * @param f Function which takes the current state as input and returns a new delta.
    * @return BFT containing one BFTDelta with the update.
    */
  def update(f: V => V)(using Byteable[V], Lattice[V], Bottom[V]): BFT[V] = {
    val newValue = f(value)

    val delta = BFTDelta(newValue, heads, BFT.hash(newValue, heads))

    BFT(Set(delta))
  }

  lazy val heads: Set[Hash] =
    deltas.filter { item => deltas.forall { a => !a.predecessors.contains(item.hash) } }.map(_.hash)

  def reverseGraph(): Map[Option[Hash], Set[BFTDelta[V]]] = {
    val reverseGraph = mutable.Map[Option[Hash], Set[BFTDelta[V]]]()

    def addToGraph(from: Option[Hash], to: BFTDelta[V]) = {
      reverseGraph.updateWith(from)(_.fold(Some(Set(to)))(it => Some(it + to)))
    }

    for delta <- deltas do {
      if delta.predecessors.isEmpty then addToGraph(None, delta)
      else for hash <- delta.predecessors do addToGraph(Some(hash), delta)
    }

    reverseGraph.toMap
  }

}

object BFT {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def apply[V](initial: V)(using Byteable[V]): BFT[V] = BFT(Set(BFTDelta(initial, Set.empty)))

  def lattice[V](using lat: Lattice[V])(using Byteable[V]): Lattice[BFT[V]] = {
    (left: BFT[V], right: BFT[V]) =>
      {
        BFT((left.deltas ++ right.deltas).filter(_.hashCorrect))
      }
  }

  def hash[V](value: V, heads: Set[Hash])(using ch: Byteable[V]): Hash =
    Hash(BFT.digest.digest(Array.concat(ch.toBytes(value) :: heads.toList.map(_.content)*)))

}
