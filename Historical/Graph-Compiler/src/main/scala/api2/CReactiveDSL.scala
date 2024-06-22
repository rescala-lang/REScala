package api2

import scala.annotation.compileTimeOnly

trait CReactive[T] {
  @compileTimeOnly("This method can only be used inside blocks that are translated to C code")
  def value: T = ???
}

class CSignal[T] extends CReactive[T]

object CSignal {
  @compileTimeOnly("This method can only be used inside blocks that are translated to C code")
  def apply[T](expr: T): CSignal[T] = ???

  @compileTimeOnly("This method can only be used inside blocks that are translated to C code")
  def source[T](init: T): CSignal[T] = ???
}

class CEvent[T] extends CReactive[Option[T]]

object CEvent {
  @compileTimeOnly("This method can only be used inside blocks that are translated to C code")
  def apply[T](expr: Option[T]): CEvent[T] = ???

  @compileTimeOnly("This method can only be used inside blocks that are translated to C code")
  def source[T]: CEvent[T] = ???
}

extension [T](inline sig: CSignal[T]) {
  inline def map[R](inline f: T => R): CSignal[R] = CSignal {
    f(sig.value)
  }

  inline def changed: CEvent[T] = CEvent { Some(sig.value) }

  inline def changedTo(value: T): CEvent[T] = CEvent {
    sig.value match {
      case `value` => Some(sig.value)
      case _       => Option.empty[T]
    }
  }

  inline def observeChange(inline f: T => Unit): CEvent[Int] = CEvent {
    f(sig.value)
    Option.empty[Int]
  }
}

extension [T](inline ev: CEvent[T]) {
  @compileTimeOnly("This method can only be used inside blocks that are translated to C code")
  inline def fold[S](init: S)(f: (S, T) => S): CSignal[S] = ???

  inline def map[R](inline f: T => R): CEvent[R] = CEvent {
    ev.value.map(f)
  }

  inline def latest(inline init: T): CSignal[T] = fold(init)((_, newV) => newV)

  inline def observe(inline f: T => Unit): CEvent[Int] = CEvent {
    ev.value.foreach(f)
    Option.empty[Int]
  }
}
