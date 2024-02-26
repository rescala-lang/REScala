package reactives.api

import reactives.core.{CreationTicket, Engine, Struct}

import reactives.meta._
import reactives.reactives.Signals

import scala.language.higherKinds

trait Api {
  type Signal[+A]
  type Event[+A]
  type Var[A] <: Signal[A]
  type Evt[A] <: Event[A]
  def Evt[A](): Evt[A]
  def Var[A](v: A): Var[A]

  def observe[A](event: Event[A])(f: A => Unit): Unit
  def now[A](signal: Signal[A]): A

  def fire[A](evt: Evt[A], value: A): Unit
  def set[A](vr: Var[A], value: A): Unit

  def disconnectE(event: Event[_]): Unit
  def disconnectS(signal: Signal[_]): Unit

  def mapS[A, B](signal: Signal[A])(f: A => B): Signal[B]
  def mapE[A, B](event: Event[A])(f: A => B): Event[B]
  def fold[A, Acc](event: Event[A])(init: Acc)(f: (Acc, A) => Acc): Signal[Acc]
  def changed[A](signal: Signal[A]): Event[A]
  def filter[A](event: Event[A])(f: A => Boolean): Event[A]
  def and[A, B, C](event: Event[A], other: Event[B])(merger: (A, B) => C): Event[C]
  def or[A, B >: A](event: Event[A], other: Event[B]): Event[B]
  def zip[A, B](event: Event[A], other: Event[B]): Event[(A, B)]
  def except[A, B](event: Event[A], other: Event[B]): Event[A]
  def change[A](signal: Signal[A]): Event[Signals.Diff[A]]
  def snapshot[A](event: Event[_], signal: Signal[A]): Signal[A]
  def switchOnce[A](event: Event[_], a: Signal[A], b: Signal[A]): Signal[A]
  def switchTo[A](event: Event[A], a: Signal[A]): Signal[A]
  def toggle[A](event: Event[_], a: Signal[A], b: Signal[A]): Signal[A]
}

object Api {
  object synchronApi extends Api {

    import reactives.Schedulers.synchron

    override type Signal[+A] = synchron.Signal[A]
    override type Event[+A]  = synchron.Event[A]
    override type Var[A]     = synchron.Var[A]
    override type Evt[A]     = synchron.Evt[A]

    override def Evt[A](): Evt[A]                                                          = synchron.Evt()
    override def Var[A](v: A): Var[A]                                                      = synchron.Var(v)
    override def mapS[A, B](signal: Signal[A])(f: (A) => B): Signal[B]                     = signal.map(f)
    override def mapE[A, B](event: Event[A])(f: (A) => B): Event[B]                        = event.map(f)
    override def fold[A, Acc](event: Event[A])(init: Acc)(f: (Acc, A) => Acc): Signal[Acc] = event.fold(init)(f)
    override def changed[A](signal: Signal[A]): Event[A]                                   = signal.changed

    override def observe[A](event: Event[A])(f: (A) => Unit): Unit = event.observe(f)
    override def now[A](signal: Signal[A]): A                      = signal.readValueOnce
    override def fire[A](evt: Evt[A], value: A): Unit              = evt.fire(value)
    override def set[A](vr: Var[A], value: A): Unit                = vr.set(value)
    override def disconnectE(event: Event[_]): Unit                = event.disconnect()
    override def disconnectS(signal: Signal[_]): Unit              = signal.disconnect()

    override def filter[A](event: Event[A])(f: A => Boolean): Event[A] = event.filter(f)
    override def and[A, B, C](event: Event[A], other: Event[B])(merger: (A, B) => C): Event[C] =
      event.and(other)(merger)
    override def or[A, B >: A](event: Event[A], other: Event[B]): Event[B]             = event || other
    override def zip[A, B](event: Event[A], other: Event[B]): Event[(A, B)]            = event.zip(other)
    override def except[A, B](event: Event[A], other: Event[B]): Event[A]              = event \ other
    override def change[A](signal: Signal[A]): Event[Signals.Diff[A]]                  = signal.change
    override def snapshot[A](event: Event[_], signal: Signal[A]): Signal[A]            = event.snapshot(signal)
    override def switchOnce[A](event: Event[_], a: Signal[A], b: Signal[A]): Signal[A] = event.switchOnce(a, b)
    override def switchTo[A](event: Event[A], a: Signal[A]): Signal[A]                 = event.switchTo(a)
    override def toggle[A](event: Event[_], a: Signal[A], b: Signal[A]): Signal[A]     = event.toggle(a, b)

  }

  class metaApi[S <: Struct](graph: DataFlowGraph)(implicit
      val reifier: Reifier[S],
      ticket: CreationTicket[S],
      engine: Engine[S]
  ) extends Api {
    override type Signal[+A] = SignalRef[A]
    override type Event[+A]  = EventRef[A]
    override type Var[A]     = VarRef[A]
    override type Evt[A]     = EvtRef[A]

    override def Evt[A](): Evt[A]                                                          = graph.createEvt()
    override def Var[A](v: A): Var[A]                                                      = graph.createVar(v)
    override def mapS[A, B](signal: Signal[A])(f: (A) => B): Signal[B]                     = signal.map(f)
    override def mapE[A, B](event: Event[A])(f: (A) => B): Event[B]                        = event.map(f)
    override def fold[A, Acc](event: Event[A])(init: Acc)(f: (Acc, A) => Acc): Signal[Acc] = event.fold(init)(f)
    override def changed[A](signal: Signal[A]): Event[A]                                   = signal.changed

    override def observe[A](event: Event[A])(f: (A) => Unit): Unit = event.observe(f)
    override def now[A](signal: Signal[A]): A                      = signal.now
    override def fire[A](evt: Evt[A], value: A): Unit              = evt.fire(value)
    override def set[A](vr: Var[A], value: A): Unit                = vr.set(value)
    override def disconnectE(event: Event[_]): Unit                = event.disconnect()
    override def disconnectS(signal: Signal[_]): Unit              = signal.disconnect()

    override def filter[A](event: Event[A])(f: A => Boolean): Event[A] = event && f
    override def and[A, B, C](event: Event[A], other: Event[B])(merger: (A, B) => C): Event[C] =
      event.and(other)(merger)
    override def or[A, B >: A](event: Event[A], other: Event[B]): Event[B]             = event || other
    override def zip[A, B](event: Event[A], other: Event[B]): Event[(A, B)]            = event.zip(other)
    override def except[A, B](event: Event[A], other: Event[B]): Event[A]              = event \ other
    override def change[A](signal: Signal[A]): Event[Signals.Diff[A]]                  = signal.change
    override def snapshot[A](event: Event[_], signal: Signal[A]): Signal[A]            = event.snapshot(signal)
    override def switchOnce[A](event: Event[_], a: Signal[A], b: Signal[A]): Signal[A] = event.switchOnce(a, b)
    override def switchTo[A](event: Event[A], a: Signal[A]): Signal[A]                 = event.switchTo(a)
    override def toggle[A](event: Event[_], a: Signal[A], b: Signal[A]): Signal[A]     = event.toggle(a, b)
  }
}
