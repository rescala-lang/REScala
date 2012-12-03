package scala.events
import scala.react.Domain

/**
 * The domain object for Scala.React which has to be imported by all REScala modules
 */
object scalareact extends Domain {
  val engine = new Engine
  val scheduler = new ManualScheduler
 }