package kofre.time

import kofre.base.{Lattice, Time}

import java.util
import scala.annotation.tailrec
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer

/** Efficient storage of a set of [[kofre.base.Time]] when most stored values are contiguous ranges. */
class ArrayRanges(
    /** Internally, ranges are stored as [begin, end) in a single array where begin is inclusive and end is exclusive.
      * Note that this is accessible to enable efficient external serialization, but any direct use of this field is discouraged.
      */
    val inner: Array[Time],
    /** Operations that combine array ranges often only know an upper bound of how many result ranges there will be.
      * To minimize copying, the inner array is created with the upper bound and stored as is.
      */
    val used: Int
) {

  override def equals(obj: Any): Boolean = obj match {
    case ar: ArrayRanges =>
      // would be nice to use the following, but does not exists on JS
      // util.Arrays.equals(inner, 0, used, ar.inner, 0, ar.used)
      val left  = inner.iterator.take(used)
      val right = ar.inner.iterator.take(ar.used)
      left sameElements right
    case other => false
  }

  override def hashCode(): Int = {
    inner.iterator.take(used).hashCode()
  }

  override def toString: String = inner.iterator.take(used).grouped(2).map {
    case (Seq(s, e)) =>
      val einc = e - 1
      if s == einc then s"$s" else s"$s:$einc"
  }.mkString("[", ", ", "]")

  def disjunct(right: ArrayRanges): Boolean = {
    if (isEmpty) return true
    if (right.isEmpty) return true

    var leftIndex = 0
    var rightIndex = 0

    while (leftIndex < used && rightIndex < right.used) {
      val leftLower = inner(leftIndex)
      val leftUpper = inner(leftIndex + 1)
      val rightLower = right.inner(rightIndex)
      val rightUpper = right.inner(rightIndex + 1)

      if (leftLower >= rightUpper) rightIndex += 2
      else if rightLower >= leftUpper then leftIndex += 2
      else return false
    }

    return true
  }


  @scala.annotation.targetName("lteq")
  def <=(right: ArrayRanges): Boolean = {
    if (isEmpty) return true
    if (right.isEmpty) return false

    var leftIndex  = 0
    var rightIndex = 0

    while (leftIndex < used) {
      if (rightIndex >= right.used) return false
      val leftLower  = inner(leftIndex)
      val leftUpper  = inner(leftIndex + 1)
      val rightLower = right.inner(rightIndex)
      val rightUpper = right.inner(rightIndex + 1)

      if (leftLower > rightUpper) rightIndex += 2
      else if (leftLower < rightLower || leftUpper > rightUpper) return false
      else leftIndex += 2
    }

    return true
  }


  def contains(x: Time): Boolean = {
    val res = java.util.Arrays.binarySearch(inner.asInstanceOf[Array[Time]], 0, used, x)
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
    union(new ArrayRanges(Array(x, x + 1), 2))

  def next: Option[Time] = Option.when(used != 0)(inner(used - 1))

  def iterator: Iterator[Time] = new Iterator[Time] {
    var pos                       = 0
    var value                     = if used == 0 then 0 else inner(0)
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

  /** Traverses both ranges simultaneously to produce output ranges.
    * Only allocates a single result array (size is the sum of the used size),
    * and traverses each input once fully.
    */
  def union(other: ArrayRanges): ArrayRanges = {
    var leftPos   = 0
    var rightPos  = 0
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

    new ArrayRanges(newInner, newInnerNextIndex)
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
      } else { // overlap after start of left until end of left
        // Add parts of left range
        includeRangeInclusive(lMin, rMin - 1) // Exclude rMin
        if (lMax < rMax) {                    // l is completely removed
          // Look at next left range
          nextLeft()
        } else { // l is only partially removed
          // increase left pointer to after right and recur
          lMin = rMax
          true
        }
      }
    ) {}

    new ArrayRanges(newInner, newInnerNextIndex)
  }

  def decomposed: Iterable[ArrayRanges] = {
    inner.view.slice(0, used).sliding(2, 2).map(r => new ArrayRanges(r.toArray, 2)).to(Iterable)
  }

}

object ArrayRanges {
  val empty: ArrayRanges = new ArrayRanges(Array.emptyLongArray, 0)
  def apply(elements: Seq[(Time, Time)]): ArrayRanges =
    elements.map((s, e) => new ArrayRanges(Array(s, e), 2)).foldLeft(ArrayRanges.empty)(_ union _)
  def elems(elems: Time*): ArrayRanges = from(elems)

  def from(it: Iterable[Time]): ArrayRanges = {
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

    new ArrayRanges(newInternal, newInternalNextIndex)
  }

  given latticeInstance: Lattice[ArrayRanges] with {
    override def decompose(a: ArrayRanges): Iterable[ArrayRanges]          = a.decomposed
    override def lteq(left: ArrayRanges, right: ArrayRanges): Boolean      = left <= right
    override def merge(left: ArrayRanges, right: ArrayRanges): ArrayRanges = left union right
  }
}
