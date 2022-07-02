package api

import clangast.WithContext
import clangast.types.CType

trait ReSource {
  def inputs: List[ReSource]

  val cType: WithContext[CType]

  val baseName: String

  def valueName: String = baseName + "_" + System.identityHashCode(this).toHexString
}
