import rescala.engines.EngineImpl
import rescala.parrp.{Backoff, ParRP}

package object rescala extends EngineImpl[ParRP, ParRP](new ParRP(new Backoff)) {
  val Events = reactives.Events
  val Signals = reactives.Signals
}
