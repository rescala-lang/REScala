package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.PermissionTree.allow
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import test.rdts.baseproperties.LatticePropertyChecks

val labelGen: Gen[String] = Gen.oneOf(Gen.oneOf('a' to 'e').map(_.toString), Gen.const("*"))

given arbPermission: Arbitrary[Permission] = Arbitrary(Gen.oneOf(ALLOW, PARTIAL))

def arbPermissionTreeMaxDepth(maxDepth: Int): Gen[PermissionTree] =
  if maxDepth == 0 then Gen.oneOf(PermissionTree.allow, PermissionTree.empty)
  else
    for {
      permission           <- Gen.oneOf(ALLOW, PARTIAL)
      numChildren          <- Gen.choose(0, 4)
      maxDepths: List[Int] <- Gen.listOfN(numChildren, Gen.choose(0, maxDepth - 1))
      children <- Gen.mapOfN(
        numChildren,
        for {
          maxDepthOfChild <- Gen.choose(0, maxDepth - 1)
          child           <- arbPermissionTreeMaxDepth(maxDepthOfChild)
          label           <- labelGen
        } yield label -> child
      )
    } yield PermissionTree(permission, children)

given arbPermissionTree: Arbitrary[PermissionTree] = Arbitrary(arbPermissionTreeMaxDepth(50))

val normalizedArbPermissionTree: Arbitrary[PermissionTree] = Arbitrary(
  arbPermissionTree.arbitrary.map(tree => tree merge tree)
)

class PermissionTreeLatticeChecks extends LatticePropertyChecks[PermissionTree]
