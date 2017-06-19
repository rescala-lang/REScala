import rescala.core.Engine
import rescala.parrp.ParRP

package object rescala extends RescalaDefaultImports[ParRP] {
  override def explicitEngine: Engine[ParRP] = rescala.Engines.parrp
}
