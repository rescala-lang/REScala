package kofre.causality.impl

import java.util
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class ArrayRanges(val inner: Array[Int]) {
  override def toString: String = inner.toSeq.toString()
  override def equals(obj: Any): Boolean = obj match
    case ar: ArrayRanges => util.Arrays.equals(inner, ar.inner)

  def contains(x: Int): Boolean = {
    val res = util.Arrays.binarySearch(inner, x)
    val pos = if res < 0 then -res - 1 else res
    if pos >= inner.length then false
    else if pos % 2 == 0
      // found a start
    then inner(pos) == x
    // found an end
    else x < inner(pos)
  }

  def add(x: Int): ArrayRanges = {
    merge(ArrayRanges(Array(x, x+1)))
  }

  def starts: Iterator[Int] = if (inner.isEmpty) Iterator.empty else Range(0, inner.length, 2).iterator.map(inner.apply)
  def ends: Iterator[Int]   = if (inner.isEmpty) Iterator.empty else Range(1, inner.length, 2).iterator.map(inner.apply)

  def merge(other: ArrayRanges): ArrayRanges = {
    var leftPos  = 0
    var rightPos = 0

    inline def lstart = inner(leftPos)
    inline def lend   = inner(leftPos + 1)
    inline def rstart = other.inner(rightPos)
    inline def rend   = other.inner(rightPos + 1)

    inline def lok = leftPos < inner.size
    inline def rok = rightPos < other.inner.size

    def findNextRange() =
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
      ListBuffer(curStart, minEnd)
    end findNextRange

    @tailrec
    def rec(acc: List[Int]): List[Int] =
      if (rok || lok) rec(findNextRange().prependToList(acc))
      else acc

    ArrayRanges(rec(Nil).toArray)

  }

  override def hashCode(): Int = {
    util.Arrays.hashCode(inner)
  }
}
