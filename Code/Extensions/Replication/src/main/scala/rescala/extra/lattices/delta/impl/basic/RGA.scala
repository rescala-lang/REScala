package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.{CContext, Dot}
import rescala.extra.lattices.delta.DotStore.{DotFun, DotPair}
import rescala.extra.lattices.delta.crdt.{ForcedWriteCRDT, RGACRDT, RGANode}
import rescala.extra.lattices.delta.crdt.RGACRDT.ForcedWriteAsUIJDLattice

class RGA[E, C: CContext](crdt: DeltaCRDT[RGACRDT.State[E, C]]) {
  def read(i: Int): Option[E] = crdt.query(RGACRDT.read(i))

  def size: Int = crdt.query(RGACRDT.size)

  def toList: List[E] = crdt.query(RGACRDT.toList)

  def insert(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insert(i, e)))

  def prepend(e: E): RGA[E, C] = insert(0, e)

  def append(e: E): RGA[E, C] = insert(size, e)

  def insertAll(i: Int, elems: Iterable[E]): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insertAll(i, elems)))

  def prependAll(elems: Iterable[E]): RGA[E, C] = insertAll(0, elems)

  def appendAll(elems: Iterable[E]): RGA[E, C] = insertAll(size, elems)

  def update(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.update(i, e)))

  def delete(i: Int): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.delete(i)))

  def purgeTombstones(): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.purgeTombstones()))

  def clear(): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.clear()))

  def processReceivedDeltas(): RGA[E, C] = new RGA(crdt.processReceivedDeltas())
}

object RGA {
  type State[E, C] = RGACRDT.State[E, C]
  type Embedded[E] = DotPair[ForcedWriteCRDT.State[GList.State[Dot]], DotFun[RGANode[E]]]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): RGA[E, C] =
    new RGA(DeltaCRDT.empty(antiEntropy))
}
