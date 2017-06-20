import rescala.RescalaDefaultImports
import rescala.core.Engine
import rescala.levelbased.{LevelBasedPropagation, SimpleStruct}

package object rescala extends RescalaDefaultImports[SimpleStruct] {
  override implicit def explicitEngine: Engine[SimpleStruct, LevelBasedPropagation[SimpleStruct]] = rescala.Engines.default
}
