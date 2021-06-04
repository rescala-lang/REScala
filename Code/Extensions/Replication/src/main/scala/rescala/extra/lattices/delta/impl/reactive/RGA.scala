package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.{CContext, Delta, Dot}
import rescala.extra.lattices.delta.DotStore.{DotFun, DotPair}
import rescala.extra.lattices.delta.crdt.{ForcedWriteCRDT, RGACRDT, RGANode}
import rescala.extra.lattices.delta.crdt.RGACRDT.ForcedWriteAsUIJDLattice

class RGA[E, C: CContext](val crdt: DeltaCRDT[RGACRDT.State[E, C]]) extends CRDTInterface[RGACRDT.State[E, C]] {
  def read(i: Int): Option[E] = crdt.query(RGACRDT.read(i))

  def size: Int = crdt.query(RGACRDT.size)

  def toList: List[E] = crdt.query(RGACRDT.toList)

  def sequence: Long = crdt.query(RGACRDT.sequence)

  def insert(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insert(i, e)))

  def prepend(e: E): RGA[E, C] = insert(0, e)

  def append(e: E): RGA[E, C] = insert(size, e)

  def insertAll(i: Int, elems: Iterable[E]): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insertAll(i, elems)))

  def prependAll(elems: Iterable[E]): RGA[E, C] = insertAll(0, elems)

  def appendAll(elems: Iterable[E]): RGA[E, C] = insertAll(size, elems)

  def update(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.update(i, e)))

  def delete(i: Int): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.delete(i)))

  def updateBy(cond: E => Boolean, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.updateBy(cond, e)))

  def deleteBy(cond: E => Boolean): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.deleteBy(cond)))

  def purgeTombstones(): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.purgeTombstones()))

  def clear(): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.clear()))

  def applyDelta(delta: Delta[RGACRDT.State[E, C]]): RGA[E, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this
    else new RGA(newCRDT)
  }

  def resetDeltaBuffer(): RGA[E, C] = new RGA(crdt.resetDeltaBuffer())
}

object RGA {
  type State[E, C] = RGACRDT.State[E, C]
  type Embedded[E] = DotPair[ForcedWriteCRDT.State[GList.State[Dot]], DotFun[RGANode[E]]]

  def apply[E, C: CContext](replicaID: String): RGA[E, C] =
    new RGA(DeltaCRDT.empty(replicaID))
}
