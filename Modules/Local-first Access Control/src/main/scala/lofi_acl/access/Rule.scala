package lofi_acl.access

case class Rule(role: String, operation: Operation, path: String)

enum Operation:
  case READ
  case WRITE

enum Permission:
  case ALLOW
  case PARTIAL
