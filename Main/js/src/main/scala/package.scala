import rescala.engines.EngineImpl
import rescala.levelbased.LevelBasedPropagationEngines.SimpleNoLock
import rescala.levelbased.SimpleStruct

package object rescala extends EngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock()) {
  val Events = reactives.Events
  val Signals = reactives.Signals
}
