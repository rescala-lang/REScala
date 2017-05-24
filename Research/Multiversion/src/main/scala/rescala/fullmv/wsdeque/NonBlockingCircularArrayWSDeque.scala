package rescala.fullmv.wsdeque

import java.util.concurrent.atomic.AtomicLong

import scala.reflect.ClassTag

trait WriteableQueue[-T] {
  def pushBottom(element: T): Unit
}

/**
  * Non-Blocking Work-Stealing Dequeue using Circular Arrays. Implemented after:
  * Chase and Lev SPAA'05 "Dynamic Circular Work-Stealing Deque"
  */
class NonBlockingCircularArrayWSDeque[T](implicit m: ClassTag[T]) extends WriteableQueue[T] {
  @volatile var bottom = 0L
  val top = new AtomicLong(0)
  @volatile var segment = new CircularArray[T](3)

  def pushBottom(element: T): Unit = {
//    println(s"[${Thread.currentThread().getName}] enqueueing $element")
    val b = bottom
    val t = top.get
    val s = segment

    val sizedS = s.ensureSize((b - t).toInt + 1, t, b)
    if(sizedS ne s) segment = sizedS

    sizedS.set(b, element)
    bottom = b + 1
  }

  def popBottom(): Option[T] = {
    val b = bottom - 1
    val s = segment
    bottom = b
    val t = top.get
    val elementCount = b - t
    if(elementCount < 0) {
      bottom = t
      None
    } else {
      val element = s.get(b)
      if(elementCount > 0) {
        Some(element)
      } else if(!top.compareAndSet(t, t + 1)) {
        bottom = t + 1
        None
      } else {
        bottom = t + 1
        Some(element)
      }
    }
  }

  def steal(): Option[T] = {
    val t = top.get
    val b = bottom
    val s = segment
    val elementCount = b - t
    if(elementCount <= 0) {
      None
    } else {
      val element = s.get(t)
      if(!top.compareAndSet(t, t + 1)) {
        None
      } else {
        Some(element)
      }
    }
  }

  override def toString: String = "WSDeque"+segment.toString(top.get, bottom)
}
