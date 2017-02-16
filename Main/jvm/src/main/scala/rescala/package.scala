import rescala.engine.Engine
import rescala.parrp.ParRP

package object rescala extends RescalaDefaultImports[ParRP] {
  override implicit def Engine: Engine[ParRP, ParRP] = rescala.Engines.parrp
}
