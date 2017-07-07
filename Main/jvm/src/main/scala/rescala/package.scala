import rescala.core.{Engine, ReSerializable}
import rescala.parrp.ParRP

package object rescala extends RescalaDefaultImports[ParRP] {
  override def explicitEngine: Engine[ParRP] = rescala.Engines.parrp
  implicit def doNotSerialize[T]: ReSerializable[T] = ReSerializable.doNotSerialize
}
