import rescala.engines.EngineImpl
import rescala.parrp.{Backoff, ParRP}

package object rescala extends EngineImpl[ParRP, ParRP]("ParRP", (_, prior) => new ParRP(new Backoff, prior)) {
  val Events = reactives.Events
  val Signals = reactives.Signals
}
