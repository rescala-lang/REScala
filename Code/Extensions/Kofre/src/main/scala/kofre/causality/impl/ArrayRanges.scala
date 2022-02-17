package kofre.causality.impl

import java.util
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import kofre.IdUtil.Time

import scala.collection.immutable.NumericRange

class ArrayRanges(val inner: Array[Time], val used: Int) {
  override def toString: String = inner.toSeq.toString()
  override def equals(obj: Any): Boolean = obj match
    case ar: ArrayRanges => util.Arrays.equals(inner, ar.inner)

  def contains(x: Time): Boolean = {
    val res = util.Arrays.binarySearch(inner, x)
    val pos = if res < 0 then -res - 1 else res
    if pos >= used then false
    else if pos % 2 == 0
      // found a start
    then inner(pos) == x
    // found an end
    else x < inner(pos)
  }

  def add(x: Time): ArrayRanges =
    merge(new ArrayRanges(Array(x, x + 1), 2))

  def starts: Iterator[Time] = if (inner.isEmpty) Iterator.empty else Range(0, inner.length, 2).iterator.map(inner.apply)
  def ends: Iterator[Time]   = if (inner.isEmpty) Iterator.empty else Range(1, inner.length, 2).iterator.map(inner.apply)

  def merge(other: ArrayRanges): ArrayRanges = {
    var leftPos  = 0
    var rightPos = 0
    var mergedPos = 0

    val merged = new Array[Time](used + other.used)

    inline def write(t: Time): Unit =
      merged(mergedPos) = t
      mergedPos += 1

    inline def lstart = inner(leftPos)
    inline def lend   = inner(leftPos + 1)
    inline def rstart = other.inner(rightPos)
    inline def rend   = other.inner(rightPos + 1)

    inline def lok = leftPos < inner.size
    inline def rok = rightPos < other.inner.size

    def findNextRange(): Unit =
      var (curStart, minEnd) =
        if (!lok) (rstart, rend)
        else if !rok || lstart < rstart then
          (lstart, lend)
        else
          (rstart, rend)

      def mergeOverlapping(): Boolean =
        var res = false
        if rok && rstart <= minEnd then { res = true; minEnd = math.max(rend, minEnd); rightPos += 2 }
        if lok && lstart <= minEnd then { res = true; minEnd = math.max(lend, minEnd); leftPos += 2 }
        res

      while mergeOverlapping() do ()
      write(curStart)
      write(minEnd)
    end findNextRange

    while (rok || lok) do findNextRange()

    new ArrayRanges(merged, mergedPos)

  }

  override def hashCode(): Int = {
    util.Arrays.hashCode(inner)
  }
}

object ArrayRanges {
  def apply(elements: Seq[(Time, Time)]): ArrayRanges =
    val content = elements.flatMap(t => Iterable(t._1, t._2)).toArray
    new ArrayRanges(content, content.length)
}
