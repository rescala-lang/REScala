package lofi_acl.access

enum PermissionTreeValidationException extends RuntimeException {
  case InvalidPathException(path: List[String])
}
