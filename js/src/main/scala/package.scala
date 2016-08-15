import rescala.engines.EngineImpl
import rescala.engines.Engines.SimpleNoLock
import rescala.graph.SimpleStruct

package object rescala extends EngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock()) {
  val Events = reactives.Events
  val Signals = reactives.Signals
}
