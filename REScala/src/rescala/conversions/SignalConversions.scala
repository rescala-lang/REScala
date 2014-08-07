package rescala.conversions

import rescala._
import rescala.events._
import scala.language.implicitConversions


/**
 * Implicit conversions to make the syntax a bit more clean.
 * Use with care since they hide a bit what is going on.
 */
object SignalConversions {
  // this is dangerous: do we want expressions to be constant or changing??
  implicit def toSignal[T](op: =>T): Signal[T] = SignalSynt((s: SignalSynt[T]) => op)

  implicit def toVal[T](const: T): Signal[T] = SignalSynt((s: SignalSynt[T]) => const)

}


object EventConversions {

  // implicitly drop event parameter if not used
  implicit def dropParam[T](ev: Event[T]) = ev.dropParam


  /**
   *  Use methods without params as handlers
   *
   *  val e = new ImperativeEvent[Unit]
   *  def m1 = { test += 1 }
   *  e += m1 _
   */
  implicit def emptyToUnit(x: ()=>Unit) = {(y: Unit) => x()}



  // One can remove parameters at all for handlers of Event[Unit]
  // Presumably dangerous ...
  // implicit def dropParam[T](h: T): Function1[Unit,T] = { _ => h}

  // val e = new ImperativeEvent[Unit]()
  // e += { x => println()}
  // e += {println()}
  // e += println()



}
