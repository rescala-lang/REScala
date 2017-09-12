import rescala.core.ReSerializable
import rescala.levelbased.SimpleStruct

package object rescala extends RescalaInterface[SimpleStruct] {
  override implicit def explicitEngine: Engines.SimpleEngine = rescala.Engines.default
  implicit def doNotSerialize[T]: ReSerializable[T] = ReSerializable.doNotSerialize

}
