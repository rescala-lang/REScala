package lofi_acl.collections

import rdts.base.Uid
import rdts.time.Dot

import scala.reflect.ClassTag

class DotMap[+V /*<: AnyRef*/ ] private (private val content: Map[Uid, IArray[V]])(using ct: ClassTag[V])
    extends Map[Dot, V]:

  override def removed(dot: Dot): DotMap[V] =
    require(dot.time >= 0 && dot.time <= Int.MaxValue)
    val idx = dot.time.asInstanceOf[Int]
    val newContent = content.updatedWith(dot.place) {
      case Some(arr) =>
        if idx == (arr.length - 1) then Some(arr.dropRight(1)) // Optimization when removing tail
        else if idx >= arr.length then Some(arr)
        else Some(arr.updated(idx, null.asInstanceOf[V]))
      case None => None
    }
    DotMap[V](newContent)

  override def updated[V1 >: V](dot: Dot, value: V1): DotMap[V1] = {
    require(dot.time >= 0 && dot.time <= Int.MaxValue)
    given ClassTag[V1] = ct.asInstanceOf[ClassTag[V1]]
    val idx            = dot.time.asInstanceOf[Int]
    val newContent = content.updatedWith(dot.place) {
      case Some(arr) =>
        if idx == arr.length then Some(arr.appended(value))
        else if idx >= arr.length
        then
          val newArr = Array.ofDim[V](idx + 1)
          arr.copyToArray(newArr)
          newArr(idx) = value.asInstanceOf[V]
          Some(IArray.unsafeFromArray(newArr))
        else
          Some(arr.updated(idx, value))
      case None =>
        val arr = Array.ofDim[V](idx + 1)
        arr(idx) = value.asInstanceOf[V]
        Some(IArray.unsafeFromArray(arr))
    }
    DotMap(newContent)
  }

  override def get(dot: Dot): Option[V] =
    require(dot.time >= 0 && dot.time <= Int.MaxValue)
    content.get(dot.place) match
      case Some(arr) =>
        if dot.time >= arr.length then None
        else Option(arr(dot.time.asInstanceOf[Int]))
      case None => None

  override def iterator: Iterator[(Dot, V)] =
    content.iterator.flatMap { (uid, arr) =>
      arr.iterator
        .zipWithIndex
        .filter((v, _) => v != null)
        .map((v, idx) => Dot(uid, idx) -> v)
    }

  override def equals(o: Any): Boolean =
    if !o.isInstanceOf[DotMap[?]] then return false
    val other = o.asInstanceOf[DotMap[?]]
    if this eq other then return true
    val otherContent = other.content
    content.keySet.union(otherContent.keySet).forall { uid =>
      val left              = content.getOrElse(uid, IArray.empty)
      val right             = otherContent.getOrElse(uid, IArray.empty)
      val (smaller, bigger) = if left.length <= right.length then (left, right) else (right, left)
      smaller.indices.forall { idx =>
        smaller(idx) == bigger(idx)
      } && bigger.indices.drop(smaller.length).forall(idx => bigger(idx) == null)
    }

object DotMap {
  def empty[V <: AnyRef: ClassTag]: DotMap[V] = DotMap(Map.empty)
  def from[V <: AnyRef: ClassTag](it: Iterable[(Dot, V)]): DotMap[V] = {
    DotMap(
      it.groupMap((dot, v) => dot.place)((dot, v) => dot.time.asInstanceOf[Int] -> v)
        .map((uid, iterable) =>
          val arr = Array.ofDim[V](iterable.maxBy(_._1)._1 + 1)
          iterable.foreach((idx, v) => arr(idx) = v)
          uid -> IArray.unsafeFromArray(arr)
        )
    )
  }
}
