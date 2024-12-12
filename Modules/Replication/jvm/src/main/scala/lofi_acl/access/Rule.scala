package lofi_acl.access

case class Rule(group: Group, operation: Operation, path: String):
  def toPathIfAllowed(groupsOfUser: Set[Group], forOperation: Operation): Option[String] =
    // A write rule implies read permissions
    if groupsOfUser.contains(group) && operation <= forOperation then Some(path)
    else None

case class Group(id: String)

enum Operation extends Ordered[Operation] {
  override def compare(that: Operation): Int = {
    (this, that) match
      case (READ, READ)   => 0
      case (READ, WRITE)  => -1
      case (WRITE, WRITE) => 0
      case (WRITE, READ)  => 1
  }

  case READ
  case WRITE
}

enum Permission:
  case ALLOW
  case PARTIAL
