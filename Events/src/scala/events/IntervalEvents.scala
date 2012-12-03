package scala.events

import scala.collection.mutable.{ListBuffer,Stack}

trait IntervalEvent[+Start, +Stop] {

  type Trace = List[Event[_]]

  def start: Event[Start]
  def end: Event[Stop]

  private lazy val realStart: Event[Start] = start && (_ => !active) && startCondition _
  private lazy val realEnd: Event[Stop] = end && (_ => active) && endCondition _

  protected[events] var deployed = false

  protected[this] var _active = false
  def active = _active

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Stop) = true

  protected[this] lazy val started = (id: Int, v: Start, reacts: ListBuffer[(() => Unit, Trace)]) => {
    _active = true
  }

  protected[this] lazy val ended = (id: Int, v: Stop, reacts: ListBuffer[(() => Unit, Trace)]) => {
    _active = false
  }

  protected[events] def deploy {
    realStart += started
    realEnd += ended
    deployed = true
  }

  protected[events] def undeploy {
    realStart -= started
    realEnd -= ended
    deployed = false
  }

}

class BetweenEvent[T,U](val start: Event[T], val end: Event[U]) extends IntervalEvent[T,U]

class ExecutionEvent[T,U] extends IntervalEvent[T,U] {

  def start: Event[T] = _start
  def end: Event[U] = _end

  private var _start: Event[T] = _
  private var _end: Event[U] = _

  trait BeforeExecution {
    this: ImperativeEvent[T] =>
    _start = this
    protected[events] abstract override def afterTrigger(t: T) {
      cflow.push(t)
    }
  }

  trait AfterExecution {
    this: ImperativeEvent[U] =>
    _end = this
    protected[events] abstract override def beforeTrigger(u: U) {
      cflow.pop
    }
  }

  private val cflow = Stack[T]()

  override def active = !cflow.isEmpty

  protected[this] override def endCondition(u: U) = cflow.size == 1

  protected[events] override def deploy {}
  protected[events] override def undeploy {}

}
