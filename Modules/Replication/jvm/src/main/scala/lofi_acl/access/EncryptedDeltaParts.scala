package lofi_acl.access

case class EncryptedDeltaParts(inner: Map[String, EncryptedDeltaParts] | Array[Byte]) {
  def isEmpty: Boolean = inner.isInstanceOf[Map[?, ?]] && inner.asInstanceOf[Map[?, ?]].isEmpty
}

object EncryptedDeltaParts {
  val empty: EncryptedDeltaParts = EncryptedDeltaParts(Map.empty)
}
