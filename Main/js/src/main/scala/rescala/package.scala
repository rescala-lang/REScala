import rescala.core.ReSerializable
import rescala.levelbased.SimpleStruct

package object rescala extends RescalaDefaultImports[SimpleStruct] {
  override implicit def explicitEngine: Engines.SimpleEngine = rescala.Engines.default
  implicit def doNotSerialize[T]: ReSerializable[T] = ReSerializable.doNotSerialize

}
