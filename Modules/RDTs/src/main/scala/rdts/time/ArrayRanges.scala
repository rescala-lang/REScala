package rdts.time

import rdts.base.{Decompose, Lattice}

import java.util

/** Efficient storage of a set of [[Time]] when most stored values are contiguous ranges. */
class ArrayRanges(
    /** Internally, ranges are stored as [begin, end) in a single array where begin is inclusive and end is exclusive.
      * Note that this is accessible to enable efficient external serialization, but any direct use of this field is discouraged.
      */
    val inner: Array[Time],
    /** Operations that combine array ranges often only know an upper bound of how many result ranges there will be.
      * To minimize copying, the inner array is created with the upper bound and stored as is.
      * This is the number of used entries in the array.
      */
    val used: Int
) {

  override def equals(obj: Any): Boolean = obj match {
    case ar: ArrayRanges =>
      if inner == ar.inner && used == ar.used then return true
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
    if this.isEmpty then return true
    if right.isEmpty then return true

    var leftIndex  = 0
    var rightIndex = 0

    while leftIndex < this.used && rightIndex < right.used do {
      val leftLower  = this.inner(leftIndex)
      val leftUpper  = this.inner(leftIndex + 1)
      val rightLower = right.inner(rightIndex)
      val rightUpper = right.inner(rightIndex + 1)

      if leftLower >= rightUpper then rightIndex += 2
      else if rightLower >= leftUpper then leftIndex += 2
      else return false
    }

    return true
  }

  /** Returns the causal prefix of this range, i.e., the first contiguous range */
  def causalPrefix: ArrayRanges = if isEmpty then this else new ArrayRanges(inner.slice(0, 2), 2)

  @scala.annotation.targetName("lteq")
  def <=(right: ArrayRanges): Boolean = {
    if isEmpty then return true
    if right.isEmpty then return false

    var leftIndex  = 0
    var rightIndex = 0

    while leftIndex < used do {
      if rightIndex >= right.used then return false
      val leftLower  = inner(leftIndex)
      val leftUpper  = inner(leftIndex + 1)
      val rightLower = right.inner(rightIndex)
      val rightUpper = right.inner(rightIndex + 1)

      if leftLower > rightUpper then rightIndex += 2
      else if leftLower < rightLower || leftUpper > rightUpper then return false
      else leftIndex += 2
    }

    return true
  }

  def contains(x: Time): Boolean = {
    val index =
      // binary search returns either the index of x, or the position where x should be inserted (but shifted into negative numbers)
      val res = java.util.Arrays.binarySearch(inner, 0, used, x)
      if res < 0 then -res - 1 else res
    if index >= used then false
    else if index % 2 == 0
    // found a start
    then inner(index) == x
    // found an end
    else x < inner(index)
  }

  def isEmpty: Boolean = used == 0

  def add(x: Time): ArrayRanges =
    union(new ArrayRanges(Array(x, x + 1), 2))

  def next: Option[Time] = Option.when(used != 0)(inner(used - 1))

  def iterator: Iterator[Time] = new Iterator[Time] {
    var pos   = 0
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

  def head: Time =
    if used == 0 then throw new NoSuchElementException()
    else inner(0)

  /** Traverses both ranges simultaneously to produce output ranges.
    * Only allocates a single result array (size is the sum of the used size),
    * and traverses each input once fully.
    */
  def union(other: ArrayRanges): ArrayRanges = {
    var leftPos   = 0
    var rightPos  = 0
    var mergedPos = 0

    inline def lstart = inner(leftPos)
    inline def lend   = inner(leftPos + 1)
    inline def rstart = other.inner(rightPos)
    inline def rend   = other.inner(rightPos + 1)

    inline def lok = leftPos < used
    inline def rok = rightPos < other.used

    // return early if one is empty
    if !lok then return other
    if !rok then return this

    val merged = new Array[Time](used + other.used)

    // fast path if left end touches right start
    if inner(used - 1) == rstart then {
      System.arraycopy(inner, 0, merged, 0, used - 1)
      System.arraycopy(other.inner, 1, merged, used - 1, other.used - 1)
      return new ArrayRanges(merged, used + other.used - 2)
    }

    // fast path if this is strictly before right
    if inner(used - 1) <= rstart then {
      System.arraycopy(inner, 0, merged, 0, used)
      System.arraycopy(other.inner, 0, merged, used, other.used)
      return new ArrayRanges(merged, used + other.used)
    }

    inline def write(t: Time): Unit =
      merged(mergedPos) = t
      mergedPos += 1

    def findNextRange(): Unit =
      var curStart = 0L
      var minEnd   = 0L

      if !lok then
        curStart = rstart
        minEnd = rend
      else if !rok || lstart < rstart then
        curStart = lstart
        minEnd = lend
      else
        curStart = rstart
        minEnd = rend

      def mergeOverlapping(): Boolean =
        var res = false
        if rok && rstart <= minEnd then { res = true; minEnd = math.max(rend, minEnd); rightPos += 2 }
        if lok && lstart <= minEnd then { res = true; minEnd = math.max(lend, minEnd); leftPos += 2 }
        res

      while mergeOverlapping() do ()
      write(curStart)
      write(minEnd)
    end findNextRange

    while rok || lok do findNextRange()

    new ArrayRanges(merged, mergedPos)

  }

  def intersect(right: ArrayRanges): ArrayRanges = {
    var newInnerNextIndex = 0
    val newInner          = new Array[Time](used + right.used)

    var lIndex = 0
    var rIndex = 0

    while lIndex < used && rIndex < right.used do {
      val lMin = inner(lIndex)
      val lMax = inner(lIndex + 1) - 1
      val rMin = right.inner(rIndex)
      val rMax = right.inner(rIndex + 1) - 1

      if lMin > rMax then {
        rIndex += 2
      } else if rMin > lMax then {
        lIndex += 2
      } else {
        val newMin: Time = Math.max(lMin, rMin)
        val newMax: Time = Math.min(lMax, rMax)

        newInner(newInnerNextIndex) = newMin         // From newMin
        newInner(newInnerNextIndex + 1) = newMax + 1 // to newMax (but range is exclusive, so +1)
        newInnerNextIndex += 2

        if newMax == rMax then rIndex += 2
        if newMax == lMax then lIndex += 2
      }
    }

    new ArrayRanges(newInner, newInnerNextIndex)
  }

  def subtract(right: ArrayRanges): ArrayRanges = {
    if right.isEmpty then return this
    if isEmpty then return this

    var newInnerNextIndex = 0
    val newInner          = new Array[Time](used + right.used)
    def includeRangeInclusive(min: Time, max: Time): Unit = {
      newInner(newInnerNextIndex) = min         // From lMin
      newInner(newInnerNextIndex + 1) = max + 1 // to lMax (but range is in array is exclusive, so lMax+1)
      newInnerNextIndex += 2
    }

    var lIndex = 0
    var lMin   = inner(0)
    var lMax   = inner(1) - 1
    def nextLeft(): Boolean = {
      lIndex += 2
      if lIndex < used then {
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
    def nextRightOrAddAllFromLeft(): Boolean = {
      rIndex += 2
      if rIndex < right.used then {
        rMin = right.inner(rIndex)
        rMax = right.inner(rIndex + 1) - 1
        true
      } else {
        // Add lMin / lMax with potential holes
        includeRangeInclusive(lMin, lMax)
        lIndex += 2
        // Add rest of left
        while lIndex < used do {
          newInner(newInnerNextIndex) = inner(lIndex)
          newInner(newInnerNextIndex + 1) = inner(lIndex + 1)
          newInnerNextIndex += 2
          lIndex += 2
        }
        false
      }
    }

    // Loop over ranges in left, creating holes for ranges that are in right
    while
      if lMin > rMax // left range is entirely after right range
      then
        // Look at next range of right
        nextRightOrAddAllFromLeft()
      else if lMax < rMin // left range is entirely before right range
      then
        // Add left range
        includeRangeInclusive(lMin, lMax)
        // Look at next range from left
        nextLeft()
      else if lMin >= rMin // left range starts after or at start of right range
      then
        if lMax > rMax // overlap from start but not until end
        then
          lMin = rMax + 1 // punch a hole in left range ending at rMax
          // Look at next range of right
          nextRightOrAddAllFromLeft()
        else // Complete overlap
          // Don't add left range
          // Look at next left range
          nextLeft()
      else // overlap after start of left until end of left
        // Add parts of left range
        includeRangeInclusive(lMin, rMin - 1) // Exclude rMin
        if lMax < rMax                        // l is completely removed
        then
          // Look at next left range
          nextLeft()
        else // l is only partially removed
          // increase left pointer to after right and recur
          lMin = rMax
          true
    do ()

    new ArrayRanges(newInner, newInnerNextIndex)
  }

  def decomposed: Iterable[ArrayRanges] = {
    inner.view.slice(0, used).sliding(2, 2).map(r => new ArrayRanges(r.toArray, 2)).to(Iterable)
  }

}

object ArrayRanges {
  val empty: ArrayRanges = new ArrayRanges(Array.emptyLongArray, 0)
  def apply(elements: Seq[(Time, Time)]): ArrayRanges =
    elements.map((s, e) => new ArrayRanges(Array(s, e), 2)).foldLeft(ArrayRanges.empty)(_ `union` _)
  def elems(elems: Time*): ArrayRanges = from(elems)

  def from(it: Iterable[Time]): ArrayRanges = {
    val sorted = it.toArray.sortInPlace() // TODO: could be optimized further, but should be fast if already sorted
    if sorted.isEmpty then return ArrayRanges.empty

    var newInternalNextIndex = 0
    val newInternal          = new Array[Time](sorted.length * 2)

    var lastMin = sorted(0)
    var lastMax = sorted(0) - 1

    for time <- sorted do {
      if time <= lastMax + 1 then {
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

  given latticeInstance: Lattice[ArrayRanges] with Decompose[ArrayRanges] with {
    extension (a: ArrayRanges)
      override def decomposed: Iterable[ArrayRanges]                       = a.decomposed
    override def subsumption(left: ArrayRanges, right: ArrayRanges): Boolean      = left <= right
    override def merge(left: ArrayRanges, right: ArrayRanges): ArrayRanges = left `union` right
  }

  def leftRightToOrder: (Boolean, Boolean) => Option[Int] =
    case (true, true)   => Some(0)
    case (true, false)  => Some(-1)
    case (false, true)  => Some(1)
    case (false, false) => None

  given partialOrder: PartialOrdering[ArrayRanges] with {
    override def lteq(x: ArrayRanges, y: ArrayRanges): Boolean = x <= y
    override def tryCompare(left: ArrayRanges, right: ArrayRanges): Option[Int] = {
      (left.isEmpty, right.isEmpty) match
        case (true, true)  => Some(0)
        case (true, false) => Some(-1)
        case (false, true) => Some(1)
        case (false, false) =>
          var leftIndex  = 0
          var rightIndex = 0

          var leftLTE  = true
          var rightLTE = true

          while
            leftIndex < left.used &&
            rightIndex < right.used &&
            (leftLTE || rightLTE)
          do
            val leftLower  = left.inner(leftIndex)
            val leftUpper  = left.inner(leftIndex + 1)
            val rightLower = right.inner(rightIndex)
            val rightUpper = right.inner(rightIndex + 1)

            if // complete right interval not known by left
              rightUpper <= leftLower
            then
              rightLTE = false
              rightIndex += 2
            else if // complete left interval not known by right
              leftUpper <= rightLower
            then
              leftLTE = false
              leftIndex += 2
            else if // intervals are exactly the same
              leftLower == rightLower && rightUpper == leftUpper
            then
              leftIndex += 2
              rightIndex += 2
            else // now we know there is some overlap, disambiguate further
            if   // right inside left
              rightUpper <= leftUpper &&
              leftLower <= rightLower
            then
              leftLTE = false
              rightIndex += 2
            else if // left inside right
              leftUpper <= rightUpper &&
              rightLower <= leftLower
            then
              rightLTE = false
              leftIndex += 2
            else // both partially overlap
              rightLTE = false
              leftLTE = false
          end while

          leftRightToOrder(leftLTE, rightLTE)
    }
  }
}
