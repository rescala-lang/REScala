package api

trait ReSource {
  def inputs: List[ReSource]
  val baseName: String
  def valueName: String = baseName + "_" + System.identityHashCode(this).toHexString
}
