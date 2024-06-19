package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import rdts.base.{Bottom, Lattice}

import scala.annotation.{tailrec, targetName}

case class PermissionTree(permission: Permission, children: Map[String, PermissionTree]) {

  /** Returns true if this is less than or equal to the PermissionTree in the argument.
    *
    * Assumes two minimized and normalized PermissionTree
    */
  @targetName("lteq")
  def <=(right: PermissionTree): Boolean =
    (this, right) match
      case (_, PermissionTree(ALLOW, _))                          => true
      case (PermissionTree(ALLOW, _), PermissionTree(PARTIAL, _)) => false
      case (PermissionTree(PARTIAL, leftChildren), PermissionTree(PARTIAL, rightChildren)) =>
        leftChildren.forall((key, leftChild) =>
          rightChildren.get(key) match
            case Some(rightChild) => leftChild <= rightChild
            case None             => false
        )
}

object PermissionTree {
  val allow: PermissionTree = PermissionTree(ALLOW, Map.empty)
  val empty: PermissionTree = PermissionTree(PARTIAL, Map.empty)

  given lattice: Lattice[PermissionTree] with
    private val childrenLattice: Lattice[Map[String, PermissionTree]] =
      Lattice.mapLattice(using (left, right) => mergeNonNormalizing(left, right))

    override def merge(left: PermissionTree, right: PermissionTree): PermissionTree =
      val merged     = mergeNonNormalizing(left, right)
      val normalized = normalizeWildcards(merged)
      normalized

    private[PermissionTree] def mergeNonNormalizing(left: PermissionTree, right: PermissionTree): PermissionTree =
      (left, right) match
        case (PermissionTree(ALLOW, _), PermissionTree(_, _)) => allow
        case (PermissionTree(_, _), PermissionTree(ALLOW, _)) => allow
        case (PermissionTree(PARTIAL, leftChildren), PermissionTree(PARTIAL, rightChildren)) =>
          PermissionTree(PARTIAL, childrenLattice.merge(leftChildren, rightChildren))

    def normalizeWildcards(tree: PermissionTree): PermissionTree = tree match
      case PermissionTree(ALLOW, _)                                                    => allow
      case PermissionTree(_, children) if children.forall((_, child) => child.isEmpty) => empty
      case PermissionTree(_, children) => children.get("*") match
          case Some(PermissionTree(ALLOW, _)) => allow // Normalize trailing * -> allow
          case Some(w) =>
            val wildcardTree = normalizeWildcards(w)
            if wildcardTree == allow then return allow
            // Merge all wildcard children into all children of siblings
            var normalizedChildren = children
              .filterNot((label, _) => label == "*") // Don't merge "*" into itself
              .map((label, child) => label -> normalizeWildcards(child))
              .filterNot(_._2.isEmpty) // Filter out empty children
              .map((label, child) =>
                label -> normalizeWildcards(mergeNonNormalizing(child, wildcardTree))
              )

            // Only add wildcard, if it is non-empty
            if !wildcardTree.isEmpty then normalizedChildren += ("*" -> wildcardTree)

            if normalizedChildren.isEmpty then empty // We filtered out empty children -> empty map
            else PermissionTree(PARTIAL, normalizedChildren)
          case None =>
            val normalizedChildren = tree.children
              .map((label, child) => label -> normalizeWildcards(child))
              .filterNot(_._2.isEmpty) // Remove empty branches
            if normalizedChildren.forall((_, child) => child.isEmpty) then empty
            else PermissionTree(PARTIAL, normalizedChildren)

  given bottom: Bottom[PermissionTree] with
    override val empty: PermissionTree = PermissionTree.empty

  // Assumes normalized trees (with wildcards merged into siblings)
  def intersect(left: PermissionTree, right: PermissionTree): PermissionTree = (left, right) match
    case (PermissionTree(ALLOW, _), PermissionTree(ALLOW, _))   => allow
    case (PermissionTree(ALLOW, _), PermissionTree(PARTIAL, _)) => right
    case (PermissionTree(PARTIAL, _), PermissionTree(ALLOW, _)) => left
    case (PermissionTree(PARTIAL, leftChildren), PermissionTree(PARTIAL, rightChildren)) =>
      val intersectionOfChildren = leftChildren.keySet.intersect(rightChildren.keySet).map(label =>
        label -> intersect(leftChildren(label), rightChildren(label))
      )
      lattice.normalizeWildcards(PermissionTree(PARTIAL, intersectionOfChildren.toMap))

  def fromPath(path: String): PermissionTree = {
    require(!path.contains("..") && path != ".")

    @tailrec
    def removeWildcardSuffix(path: String): String =
      val stripped = path.stripSuffix(".*")
      if stripped eq path then path
      else removeWildcardSuffix(stripped)

    val shortenedPath = removeWildcardSuffix(path)
    if shortenedPath == "*" || shortenedPath == "" then return allow

    val tree = shortenedPath.split('.').foldRight(allow) {
      case (pathElement, childTree) => PermissionTree(PARTIAL, Map(pathElement -> childTree))
    }

    lattice.normalizeWildcards(tree) // Ensure normalization
  }

  def fromPathSet(paths: Set[String]): PermissionTree = {
    val mergedTree = paths.foldLeft(empty) { (permissionTree, path) =>
      lattice.mergeNonNormalizing(permissionTree, fromPath(path))
    }
    lattice.normalizeWildcards(mergedTree)
  }

}
