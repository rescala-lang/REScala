package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.PermissionTree.allow
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import test.rdts.baseproperties.LatticePropertyChecks

val labelGen: Gen[String] = Gen.oneOf(Gen.oneOf('a' to 'e').map(_.toString), Gen.const("*"))

given arbPermission: Arbitrary[Permission] = Arbitrary(Gen.oneOf(ALLOW, PARTIAL))

def arbPermissionTreeMaxDepth(maxDepth: Int): Gen[PermissionTree] =
  if maxDepth == 0 then Gen.oneOf(PermissionTree.allow, PermissionTree.empty)
  else
    for
      permission           <- Gen.oneOf(ALLOW, PARTIAL)
      numChildren          <- Gen.choose(0, 4)
      maxDepths: List[Int] <- Gen.listOfN(numChildren, Gen.choose(0, maxDepth - 1))
      children <- Gen.mapOfN(
        numChildren,
        for
          maxDepthOfChild <- Gen.choose(0, maxDepth - 1)
          child           <- arbPermissionTreeMaxDepth(maxDepthOfChild)
          label           <- labelGen
        yield label -> child
      )
    yield PermissionTree(permission, children)

given arbPermissionTree: Arbitrary[PermissionTree] = Arbitrary(arbPermissionTreeMaxDepth(50))

val normalizedArbPermissionTree: Arbitrary[PermissionTree] = Arbitrary(
  arbPermissionTree.arbitrary.map(tree => tree `merge`tree)
)

class PermissionTreeLatticeChecks extends LatticePropertyChecks[PermissionTree](orderAgreesWithStructuralEquals = false)

class PermissionTreePropertyChecks extends munit.ScalaCheckSuite {
  def checkAllLeafsAreAllow(tree: PermissionTree): Unit = tree match
    case PermissionTree(permission, children) =>
      assert(children.nonEmpty || permission == ALLOW)
      children.foreach((_, child) => checkAllLeafsAreAllow(child))

  def checkOnlyLeafsAreAllow(tree: PermissionTree): Unit = tree match
    case PermissionTree(ALLOW, children)   => assert(children.isEmpty)
    case PermissionTree(PARTIAL, children) => children.foreach((_, child) => checkOnlyLeafsAreAllow(child))

  property("All non-empty normalized trees only have allow leafs and no allow in branching nodes") {
    forAll { (tree: PermissionTree) =>
      val normalized = Lattice[PermissionTree].normalize(tree)
      if !normalized.isEmpty then
        checkOnlyLeafsAreAllow(normalized)
        checkAllLeafsAreAllow(normalized)
    }
  }

  def inNormalizedTreeAllSiblingsContainWildcardChildren(tree: PermissionTree): Unit =
    def contains(tree: PermissionTree, wildcardBranch: Map[String, PermissionTree]): Boolean =
      tree match
        case PermissionTree(ALLOW, _) => true
        case PermissionTree(PARTIAL, treeChildren) => wildcardBranch.forall((wLabel, wChild) =>
            treeChildren.contains(wLabel) && contains(treeChildren(wLabel), wChild.children)
          )

    tree.children.get("*") match
      case Some(wChild) =>
        tree.children.filterNot(_._1 == "*").foreach((_, sibling) => require(contains(sibling, wChild.children)))
      case None =>

    tree.children.foreach((label, subtree) => inNormalizedTreeAllSiblingsContainWildcardChildren(subtree))

  property("Non-wildcard siblings of wildcards are at least as permissive as wildcard sibling") {
    forAll { (tree: PermissionTree) =>
      val normalized = Lattice[PermissionTree].normalize(tree)
      inNormalizedTreeAllSiblingsContainWildcardChildren(normalized)
    }
  }
}
