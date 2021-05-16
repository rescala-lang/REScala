package rescala.core

object TestAPI:

  import SimpleScheduler._

  type State[T] = SimpleState[T]

  class ProtoVar[T](initState: SimpleState[T]) extends ReSource with Interp[T]:
    outer =>

    override type Value = T
    override protected[rescala] def state: State[Value]        = initState
    override protected[rescala] def name: ReName               = "I am a source name"
    override def interpret(v: Value): T                             = v
    override protected[rescala] def commit(base: Value): Value = base

    def makeChange(newValue: T) =
      new InitialChange {
        override val source = outer
        override def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean = {
          if (base != newValue) {
            writeCallback(newValue.asInstanceOf[source.Value])
            true
          } else false
        }
      }

    def set(value: T) =
      SimpleScheduler.forceNewTransaction(this) { _.recordChange(this.makeChange(value)) }

    def map[A](f: T => A): ProtoSignal[A, T] =
      CreationTicket.fromScheduler(scheduler)
        .create[A, ProtoSignal[A, T]](
          Set(this),
          f(state.value),
          inite = false
        ) { createdState =>
          new ProtoSignal(createdState, this, f)
        }

  object ProtoVar:
    def apply[V](value: V) =
      CreationTicket.fromScheduler(SimpleScheduler)
        .createSource(value) { createdState =>
          new ProtoVar[V](createdState)
        }

  class ProtoSignal[A, B](
      initState: State[A],
      inputSource: Interp[B],
      fun: B => A
  ) extends Derived with Interp[A]:
    override type Value = A
    override protected[rescala] def state: State[Value]        = initState
    override protected[rescala] def name: ReName               = "I am a name"
    override protected[rescala] def commit(base: Value): Value = base

    override protected[rescala] def reevaluate(input: ReIn): Rout = {
      val sourceVal = input.dependStatic(inputSource)
      input.withValue(fun(sourceVal))
    }

    override def interpret(v: Value): A = v

    def now = SimpleScheduler.forceNewTransaction(this) { _.now(this) }

    def observe(f: A => Unit) = Observe.strong(this, true)(value => new Observe.ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = false
      override def execute(): Unit = f(value)
    })(CreationTicket.fromScheduler(SimpleScheduler))


import TestAPI._
import SimpleScheduler._

object Test:
  @main def main(): Unit =
    val a = ProtoVar("Hello World!")
    val b = a.map(x => x + " appendage")
    b.observe(println)
    a.set("ahahaha")
