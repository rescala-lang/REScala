package rescala.pipelining

import rescala.graph._
import rescala.propagation.Turn
import rescala.twoversion.Committable

private[pipelining] object ValueHolder {

  def initStable[T](initial: T, buffer: PipelineBuffer[T]): ValueHolder[T] = {
    val holder = new ValueHolder(initial, buffer)
    holder.committedValue = Some(initial)
    holder
  }

  def initStable[T](initial: T, existing: ValueHolder[T]): Unit = {
    existing.value = initial
    existing.committedValue = Some(initial)
  }

  def initDuplicate[T](from: ValueHolder[T])(implicit newTurn: Turn[_]): ValueHolder[T] = {
    val initValue = if (from.buffer.isInstanceOf[BlockingPipelineBuffer[_]])
      from.committedValue.getOrElse(from.value)
    else from.value
    val holder = new ValueHolder(initValue, from.buffer)
    holder
  }

}

private[pipelining] class ValueHolder[T](initial: T, val buffer: PipelineBuffer[T]) {

  var value: T = initial
  var committedValue: Option[T] = None
  var isChanged = false

  def transform(f: T => T) = value = f(value)

  private def printValue() = if (committedValue.isDefined) s"Comm(${committedValue.get})" else s"Val(${value})"

  protected[pipelining] def print() = {
    if (value.isInstanceOf[Pulse[_]])
      Some(s"Pulse($printValue)")
    else if (value.isInstanceOf[Set[_]]) {
      if (buffer.isInstanceOf[BlockingPipelineBuffer[_]])
        Some(s"Incoming($printValue)")
      else Some(s"Outgoing($printValue)")
    } else None
  }

}

private[pipelining] class BufferFrameContent {

  var values: List[ValueHolder[_]] = List()

  def valueForBuffer[T](buf: PipelineBuffer[T]): ValueHolder[T] = values.find { _.buffer eq buf }.get.asInstanceOf[ValueHolder[T]] // Cast is safe

  def duplicate(newTurn: Turn[_]) = {
    val newContent = new BufferFrameContent
    for (v <- values) {
      newContent.values :+= ValueHolder.initDuplicate(v)(newTurn)
    }
    newContent
  }

  override def toString() = values.map(_.print).filter(_.isDefined).map(_.get).mkString(",")

}

private[pipelining] abstract class PipelineBuffer[A](parent: Pipeline, initialStrategy: (A, A) => A) extends Buffer[A] with Committable {

  var commitStrategy: (A, A) => A = initialStrategy

  def transform(f: (A) => A)(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn[_]): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val frame = parent.needFrame()
    frame.synchronized {
      val valueHolder = frame.content.valueForBuffer(this)
      valueHolder.value = value
      valueHolder.isChanged = true
    }
  }
  override def release(implicit turn: Turn[_]): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val frame = parent.needFrame()
    frame.content.valueForBuffer(this).isChanged = false
  }

  override def commit(implicit turn: Turn[_]): Unit = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val frame = parent.needFrame()
    val commitValue = if (frame.content.valueForBuffer(this).isChanged) {
      val oldValue = base
      val currentValue = get
      val commitValue = commitStrategy(oldValue, currentValue)
      commitValue
    } else {
      frame.content.valueForBuffer(this).isChanged = true
      parent.waitUntilCanWrite
      val oldValue = base
      val commitValue = commitStrategy(oldValue, oldValue)
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

  override def toString() = s"PipelineBuffer(${parent})"

}

private[pipelining] class NonblockingPipelineBuffer[A](parent: Pipeline, initialStrategy: (A, A) => A) extends PipelineBuffer[A](parent, initialStrategy) {

  override def base(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.findFrame {
      _ match {
        case Some(frame) =>
          val readFrane = if (frame.content.valueForBuffer(this).isChanged)
            frame.previous()
          else
            frame
          readFrane.content.valueForBuffer(this).value
        case None =>
          parent.frame().valueForBuffer(this).value

      }
    }

  }
  override def get(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.frame().valueForBuffer(this).value
  }

  override def toString() = s"NonblockingPipelineBuffer(${parent})"

}

private[pipelining] class BlockingPipelineBuffer[A](parent: Pipeline, initialStrategy: (A, A) => A) extends PipelineBuffer[A](parent, initialStrategy) {

  override def set(value: A)(implicit turn: Turn[_]): Unit = {
    assert(!parent.needFrame()(turn.asInstanceOf[PipeliningTurn]).isWritten, s"Frame at ${parent} already written when tried to set from $turn")
    super.set(value)
  }

  override def base(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.waitUntilCanRead(pTurn)

    parent.findFrame {
      _ match {
        case Some(frame) =>
          val readFrame = frame.previous()
          assert(readFrame.isWritten, s"base called for ${this.parent} during $turn without written predecessor frame")
          readFrame.content.valueForBuffer(this).committedValue.get
        case None =>
          parent.frame().valueForBuffer(this).committedValue.get
      }
    }

  }

  protected[pipelining] def forceTransform(f: (A) => A)(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    val value = f(forceGet)
    set(value)
    value
  }

  protected[pipelining] def forceGet(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]
    parent.findFrame {
      _ match {
        case Some(frame) =>
          val hasValue = frame.content.valueForBuffer(this).isChanged || frame.isWritten
          if (!hasValue) {
              frame.previous().content.valueForBuffer(this).value
          } else {
            frame.content.valueForBuffer(this).value
          }
        case None =>
          parent.frame().valueForBuffer(this).value
      }

    }
  }

  override def get(implicit turn: Turn[_]): A = {
    implicit val pTurn = turn.asInstanceOf[PipeliningTurn]

    parent.waitUntilCanRead(pTurn)

    parent.findFrame {
      _ match {
        case Some(frame) =>
          val hasValue = frame.content.valueForBuffer(this).isChanged || frame.isWritten
          if (!hasValue) {
            val prevFrame = frame.previous
            assert(prevFrame.isWritten, s"${Thread.currentThread().getId}: Frame for get at previous frame ${parent} not written during $turn: prevFrame")
            prevFrame.content.valueForBuffer(this).committedValue.get
          } else {
            frame.content.valueForBuffer(this).value
          }
        case None =>
          val frame = parent.frame
          assert(frame.isWritten, s"${Thread.currentThread().getId}: Frame for get at ${parent} not written during $turn: $frame")
          frame.content.valueForBuffer(this).committedValue.get
      }

    }
  }

  override def toString() = s"BlockingPipelineBuffer(${parent})"

}
