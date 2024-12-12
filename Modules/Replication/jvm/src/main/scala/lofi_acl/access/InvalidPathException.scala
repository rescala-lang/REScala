package lofi_acl.access

case class InvalidPathException(path: List[String]) extends RuntimeException {
  override def toString: String = s"InvalidPathException: ${path.mkString(".")}"
}
