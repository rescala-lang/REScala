package kofre.causality.impl

import java.util
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import kofre.Defs.Time
import kofre.Lattice

import scala.collection.IndexedSeqView

case class ArrayRanges(inner: Array[Time], used: Int) {

  override def equals(obj: Any): Boolean = obj match
    case ar: ArrayRanges => inner.iterator.take(used).sameElements(ar.inner.iterator.take(ar.used))

  override def toString: String = s"ArrayRanges(${inner.toSeq.take(used).toString()})"

  def contains(x: Time): Boolean = {
    val res = java.util.Arrays.binarySearch(inner, 0, used, x)
    val pos = if res < 0 then -res - 1 else res
    if pos >= used then false
    else if pos % 2 == 0
    // found a start
    then inner(pos) == x
    // found an end
    else x < inner(pos)
  }

  def isEmpty: Boolean = used == 0

  def add(x: Time): ArrayRanges =
    merge(new ArrayRanges(Array(x, x + 1), 2))

  def next: Option[Time] = Option.when(used != 0)(inner(used - 1))

  def iterator: Iterator[Time] = new Iterator[Time] {
    var pos = 0
    var value = if used == 0 then 0 else inner(0)
    override def hasNext: Boolean = used > pos
    override def next(): Time =
      val res = value
      value += 1
      if value >= inner(pos + 1) then
        pos += 2
        if used > pos then
          value = inner(pos)
      res
  }

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

    inline def lok = leftPos < used
    inline def rok = rightPos < other.used

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



  def intersect(right: ArrayRanges): ArrayRanges = {
    var newInnerNextIndex = 0
    val newInner          = new Array[Time](used + right.used)

    var lIndex = 0
    var rIndex = 0

    while (lIndex < used && rIndex < right.used) {
      val lMin = inner(lIndex)
      val lMax = inner(lIndex + 1) - 1
      val rMin = right.inner(rIndex)
      val rMax = right.inner(rIndex + 1) - 1

      if (lMin > rMax) {
        rIndex += 2
      } else if (rMin > lMax) {
        lIndex += 2
      } else {
        val newMin: Time = Math.max(lMin, rMin)
        val newMax: Time = Math.min(lMax, rMax)

        newInner(newInnerNextIndex) = newMin         // From newMin
        newInner(newInnerNextIndex + 1) = newMax + 1 // to newMax (but range is exclusive, so +1)
        newInnerNextIndex += 2

        if (newMax == rMax) rIndex += 2
        if (newMax == lMax) lIndex += 2
      }
    }

    ArrayRanges(newInner, newInnerNextIndex)
  }

  def subtract(right: ArrayRanges): ArrayRanges = {
    if (right.isEmpty) return this
    if (isEmpty) return this

    var newInnerNextIndex = 0
    val newInner          = new Array[Time](used + right.used)
    @inline def includeRangeInclusive(min: Time, max: Time): Unit = {
      newInner(newInnerNextIndex) = min         // From lMin
      newInner(newInnerNextIndex + 1) = max + 1 // to lMax (but range is in array is exclusive, so lMax+1)
      newInnerNextIndex += 2
    }

    var lIndex = 0
    var lMin   = inner(0)
    var lMax   = inner(1) - 1
    @inline def nextLeft(): Boolean = {
      lIndex += 2
      if (lIndex < used) {
        lMin = inner(lIndex)
        lMax = inner(lIndex + 1) - 1
        true
      } else {
        false
      }
    }

    var rIndex = 0
    var rMin   = right.inner(0)
    var rMax   = right.inner(1) - 1
    @inline def nextRightOrAddAllFromLeft(): Boolean = {
      rIndex += 2
      if (rIndex < right.used) {
        rMin = right.inner(rIndex)
        rMax = right.inner(rIndex + 1) - 1
        true
      } else {
        // Add lMin / lMax with potential holes
        includeRangeInclusive(lMin, lMax)
        lIndex += 2
        // Add rest of left
        while (lIndex < used) {
          newInner(newInnerNextIndex) = inner(lIndex)
          newInner(newInnerNextIndex + 1) = inner(lIndex + 1)
          newInnerNextIndex += 2
          lIndex += 2
        }
        false
      }
    }

    // Loop over ranges in left, creating holes for ranges that are in right
    while (
      if (lMin > rMax) { // left range is entirely after right range
        // Look at next range of right
        nextRightOrAddAllFromLeft()
      } else if (lMax < rMin) { // left range is entirely before right range
        // Add left range
        includeRangeInclusive(lMin, lMax)
        // Look at next range from left
        nextLeft()
      } else if (lMin >= rMin) { // left range starts after or at start of right range
        if (lMax > rMax) {       // overlap from start but not until end
          lMin = rMax + 1        // punch a hole in left range ending at rMax
          // Look at next range of right
          nextRightOrAddAllFromLeft()
        } else { // Complete overlap
          // Don't add left range
          // Look at next left range
          nextLeft()
        }
      } else if (lMin < rMin) { // overlap after start of left until end of left
        // Add parts of left range
        includeRangeInclusive(lMin, rMin - 1) // Exclude rMin
        // Look at next left range
        nextLeft()
      } else {
        throw new IllegalStateException()
      }
    ) {}

    ArrayRanges(newInner, newInnerNextIndex)
  }

  override def hashCode(): Int = {
    inner.take(used).hashCode()
  }
}

object ArrayRanges {
  val empty: ArrayRanges = new ArrayRanges(Array.empty[Time], 0)
  def apply(elements: Seq[(Time, Time)]): ArrayRanges =
    val content = elements.flatMap(t => Iterable(t._1, t._2)).toArray
    new ArrayRanges(content, content.length)

  def from(it: Iterator[Time]): ArrayRanges = {
    val sorted = it.toArray.sortInPlace() // TODO: could be optimized further, but should be fast if already sorted
    if (sorted.isEmpty) return ArrayRanges.empty

    var newInternalNextIndex = 0
    val newInternal          = new Array[Time](sorted.length * 2)

    var lastMin = sorted(0)
    var lastMax = sorted(0) - 1

    for (time <- sorted) {
      if (time <= lastMax + 1) {
        lastMax = time
      } else {
        newInternal(newInternalNextIndex) = lastMin         // from lastMin
        newInternal(newInternalNextIndex + 1) = lastMax + 1 // until lastMax (exclusive)
        newInternalNextIndex += 2
        lastMin = time
        lastMax = time
      }
    }

    newInternal(newInternalNextIndex) = lastMin         // from lastMin
    newInternal(newInternalNextIndex + 1) = lastMax + 1 // until lastMax (exclusive)
    newInternalNextIndex += 2

    ArrayRanges(newInternal, newInternalNextIndex)
  }

  given latticeInstance: Lattice[ArrayRanges] = _ merge _
}
