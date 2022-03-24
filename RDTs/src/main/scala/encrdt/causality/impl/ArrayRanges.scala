package de.ckuessner
package encrdt.causality.impl

import encrdt.causality.impl.Defs.Time
import encrdt.lattices.SemiLattice

import scala.collection.mutable

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

  def from(it: Iterator[Time]): ArrayRanges = {
    val sorted = it.toArray.sortInPlace() // TODO: could be optimized further, but should be fast if already sorted
    if (sorted.isEmpty) return ArrayRanges.empty

    val newInternal = mutable.ArrayBuilder.make[Time]

    var lastMin = sorted(0)
    var lastMax = sorted(0) - 1

    for (time <- sorted) {
      if (time == lastMax + 1) {
        lastMax = time
      } else {
        newInternal += lastMin
        newInternal += lastMax
        lastMin = time
        lastMax = time
      }
    }

    newInternal += lastMin
    newInternal += lastMax

    ArrayRanges(newInternal.result(), newInternal.length)
  }

  def intersect(left: ArrayRanges, right: ArrayRanges): ArrayRanges = {
    val newInner = mutable.ArrayBuilder.make[Time]

    var lIndex = 0
    var rIndex = 0

    while (lIndex < left.used && rIndex < right.used) {
      val lMin = left.inner(lIndex)
      val lMax = left.inner(lIndex + 1)
      val rMin = right.inner(rIndex)
      val rMax = right.inner(rIndex + 1)

      if (lMin > rMax) {
        rIndex += 2
      } else if (rMin > lMax) {
        lIndex += 2
      } else {
        val newMin: Time = Math.max(lMin, rMin)
        val newMax: Time = Math.min(lMax, rMax)

        newInner += newMin
        newInner += newMax

        if (newMax == rMax) rIndex += 2
        if (newMax == lMax) lIndex += 2
      }
    }

    ArrayRanges(newInner.result(), newInner.length)
  }

  implicit val latticeInstance: SemiLattice[ArrayRanges] = _ merge _
}
