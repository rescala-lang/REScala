package scala.events.reactive
import scala.events.ImperativeEvent
import scala.events.VarList
import scala.events.EventNode

/*
 * This is an artifact from a very old version. Somehow it hooks deep into the EScala dependencies.
 */
trait Reactive[T] {	
	val invalidated = new ImperativeEvent[Unit]
	def apply(): T
}
