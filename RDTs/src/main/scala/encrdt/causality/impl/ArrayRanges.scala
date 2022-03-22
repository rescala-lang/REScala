package de.ckuessner
package causality.impl

import de.ckuessner.causality.impl.Defs.Time
import de.ckuessner.encrdt.lattices.SemiLattice

case class ArrayRanges(inner: Array[Time], used: Int) {

  override def equals(obj: Any): Boolean = obj match {
    case ar: ArrayRanges => inner.iterator.take(used).sameElements(ar.inner.iterator.take(ar.used))
  }

  override def toString: String = s"ArrayRanges(${inner.toSeq.take(used).toString()})"

  def contains(x: Time): Boolean = {
    val res = java.util.Arrays.binarySearch(inner, 0, used, x)
    val pos = if (res < 0) -res - 1 else res
    if (pos >= used) false
    else if (pos % 2 == 0)
      // found a start
      inner(pos) == x
      // found an end
    else x < inner(pos)
  }

  def add(x: Time): ArrayRanges =
    merge(new ArrayRanges(Array(x, x + 1), 2))

  def next: Option[Time] = Option.when(used != 0)(inner(used - 1))

  def iterator: Iterator[Time] = new Iterator[Time] {
    var pos                       = 0
    var value: Time               = if (used == 0) 0 else inner(0)
    override def hasNext: Boolean = used > pos
    override def next(): Time = {
      val res = value
      value += 1
      if (value >= inner(pos + 1)) {
        pos += 2
        if (used > pos) value = inner(pos)
      }
      res
    }
  }

  def merge(other: ArrayRanges): ArrayRanges = {
    var leftPos   = 0
    var rightPos  = 0
    var mergedPos = 0

    val merged = new Array[Time](used + other.used)

    @inline def write(t: Time): Unit = {
      merged(mergedPos) = t

      mergedPos += 1
    }

    @inline def lstart = inner(leftPos)
    @inline def lend   = inner(leftPos + 1)
    @inline def rstart = other.inner(rightPos)
    @inline def rend   = other.inner(rightPos + 1)

    @inline def lok = leftPos < used
    @inline def rok = rightPos < other.used

    def findNextRange(): Unit = {
      var (curStart, minEnd) =
        if (!lok) (rstart, rend)
        else if (!rok || lstart < rstart)
          (lstart, lend)
        else
          (rstart, rend)

      def mergeOverlapping(): Boolean = {
        var res = false
        if (rok && rstart <= minEnd) { res = true; minEnd = math.max(rend, minEnd); rightPos += 2 }
        if (lok && lstart <= minEnd) { res = true; minEnd = math.max(lend, minEnd); leftPos += 2 }
        res
      }

      while (mergeOverlapping()) ()
      write(curStart)
      write(minEnd)
    }

    while (rok || lok) findNextRange()

    new ArrayRanges(merged, mergedPos)

  }

  override def hashCode(): Int = {
    inner.take(used).hashCode()
  }
}

object ArrayRanges {
  val empty: ArrayRanges = new ArrayRanges(Array.empty[Time], 0)
  def apply(elements: Seq[(Time, Time)]): ArrayRanges = {
    val content = elements.flatMap(t => Iterable(t._1, t._2)).toArray
    new ArrayRanges(content, content.length)
  }

  // this is horrible in performance, please fix
  def from(it: Iterator[Time]): ArrayRanges =
    it.foldLeft(ArrayRanges.empty)((range, time) => range.add(time))

  implicit val latticeInstance: SemiLattice[ArrayRanges] = _ merge _
}
