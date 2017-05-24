package rescala.fullmv.wsdeque

import scala.reflect.ClassTag

class CircularArray[T](val sizeLog: Int)(implicit m: ClassTag[T]) {
  val size = 1 << sizeLog
  val segment = new Array[T](size)

  def get(i: Long): T = {
    segment(moduloIndex(i))
  }

  def set(i: Long, e: T): Unit = {
    segment(moduloIndex(i)) = e
  }

  private def moduloIndex(i: Long) = {
    (i % size).toInt
  }

  def ensureSize(size: Int, top: Long, bottom: Long): CircularArray[T] = {
    if(this.size > size) {
      this
    } else {
      val newSize = (math.log(size.toDouble)/math.log(2d)).toInt + 1
      val grown = new CircularArray[T](newSize)
      for (i <- top until bottom) {
        grown.set(i, get(i))
      }
      grown
    }
  }

  def toString(top: Long, bottom: Long): String = {
    val t = moduloIndex(top)
    val b = moduloIndex(bottom)
    "(" + (if (t < b) {
      segment.slice(t, b)
    } else {
      segment.drop(size - t) ++ segment.take(b)
    }).mkString(", ") + ")"
  }
}
