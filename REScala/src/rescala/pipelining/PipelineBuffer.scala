package rescala.pipelining

import rescala.turns.Turn
import scala.collection.immutable.Queue
import scala.annotation.tailrec

import rescala.graph._

object ValueHolder {

  def initStable[T](initial: T, buffer: PipelineBuffer[T]): ValueHolder[T] = {
    val holder = new ValueHolder(initial, buffer)
    holder.committedValue = Some(initial)
    holder
  }

  def initStable[T](initial: T, existing: ValueHolder[T]): Unit = {
    existing.value = initial
    existing.committedValue = Some(initial)
  }

  def initDuplicate[T](from: ValueHolder[T])(implicit newTurn: Turn): ValueHolder[T] = {
    val holder = new ValueHolder(from.value, from.buffer)
    newTurn.schedule(holder.buffer)
    holder
  }

}

class ValueHolder[T](initial: T, val buffer: PipelineBuffer[T]) {

  var value: T = initial
  var committedValue: Option[T] = None
  var isChanged = false

  def transform(f: T => T) = value = f(value)

}

class BufferFrameContent {

  var values: List[ValueHolder[_]] = List()

  def valueForBuffer[T](buf: PipelineBuffer[T]): ValueHolder[T] = values.find { _.buffer eq buf }.get.asInstanceOf[ValueHolder[T]] // Cast is safe

  def duplicate(newTurn: Turn) = {
    val newContent = new BufferFrameContent
    for (v <- values) {
      newContent.values :+= ValueHolder.initDuplicate(v)(newTurn)
    }
    newContent
  }

}

abstract class PipelineBuffer[A](parent: Pipeline, initialStrategy: (A, A) => A) extends Buffer[A] {

  var commitStrategy: (A, A) => A = initialStrategy

  override def initCurrent(value: A): Unit = ValueHolder.initStable(value, parent.getStableFrame().valueForBuffer(this))
  override def initStrategy(strategy: (A, A) => A): Unit = synchronized(commitStrategy = strategy)

  override def transform(f: (A) => A)(implicit turn: Turn): A = synchronized {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val value = f(get)
    set(value)
    value
  }

  private def setNotSchedule(value: A)(implicit turn: Turn): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val frame = parent.needFrame()
    println(frame)
    assert(!frame.isWritten)
    val valueHolder = frame.content.valueForBuffer(this)
    valueHolder.value = value
    valueHolder.isChanged = true

  }

  override def set(value: A)(implicit turn: Turn): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    setNotSchedule(value)
    turn.schedule(this)
  }
  override def release(implicit turn: Turn): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val frame = parent.needFrame()
    frame.content.valueForBuffer(this).isChanged = false
  }

  override def commit(implicit turn: Turn): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val frame = parent.needFrame()
    val commitValue = if (frame.content.valueForBuffer(this).isChanged) {
      val oldValue = base
      val currentValue = get
      val commitValue = commitStrategy(oldValue, currentValue)
      //setNotSchedule(commitValue)
      commitValue
    } else {
      frame.content.valueForBuffer(this).isChanged = true
      parent.waitUntilCanWrite
      val oldValue = base
      val commitValue = commitStrategy(oldValue, oldValue)
      // setNotSchedule(commitValue)
      commitValue
    }

    assert(!frame.isWritten)
    val valueHolder = frame.content.valueForBuffer(this)
    //   valueHolder.value = commitValue
    valueHolder.committedValue = Some(commitValue)
    valueHolder.isChanged = true

    release
    // assert(get == requiredValue)
  }

  override def toString() = s"PipelineBuffer(${parent.reactive})"

}

class NonblockingPipelineBuffer[A](parent: Pipeline, initialStrategy: (A, A) => A) extends PipelineBuffer[A](parent, initialStrategy) {

  override def base(implicit turn: Turn): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.findFrame {
      _ match {
        case Some(frame) =>
          val content = if (frame.content.valueForBuffer(this).isChanged)
            if (frame.previous() == null)
              parent.getStableFrame()
            else
              frame.previous().content
          else
            frame.content
          content.valueForBuffer(this).value
        case None =>
          parent.frame().valueForBuffer(this).value

      }
    }

  }
  override def get(implicit turn: Turn): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.frame().valueForBuffer(this).value
  }

  override def toString() = s"NonblockingPipelineBuffer(${parent.reactive})"

}

class BlockingPipelineBuffer[A](parent: Pipeline, initialStrategy: (A, A) => A) extends PipelineBuffer[A](parent, initialStrategy) {

  override def base(implicit turn: Turn): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.findFrame {
      _ match {
        case Some(frame) =>

          val content = if (frame.previous() == null)
            parent.getStableFrame()
          else
            frame.previous().content

          content.valueForBuffer(this).committedValue.get
        case None =>
          parent.frame().valueForBuffer(this).committedValue.get
      }
    }

  }
  override def get(implicit turn: Turn): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    if (turn.isInstanceOf[PipeliningTurn])
      parent.waitUntilCanRead(pTurn)

    parent.findFrame {
      _ match {
        case Some(frame) =>
          val hasValue = frame.content.valueForBuffer(this).isChanged || frame.isWritten
          if (!hasValue) {
            if (frame.previous() == null)
              parent.getStableFrame().valueForBuffer(this).committedValue.get
            else
              frame.previous().content.valueForBuffer(this).committedValue.get
          } else {
            frame.content.valueForBuffer(this).value
          }
        case None =>
          parent.frame().valueForBuffer(this).committedValue.get
      }

    }
  }

  override def toString() = s"BlockingPipelineBuffer(${parent.reactive})"

}