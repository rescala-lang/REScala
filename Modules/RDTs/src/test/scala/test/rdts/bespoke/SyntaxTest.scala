package test.rdts.bespoke

import rdts.base.Lattice
import rdts.base.Uid.asId
import rdts.datatypes.contextual.EnableWinsFlag
import rdts.dotted.HasDots.*
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.LocalReplicaId
import rdts.time.{Dot, Dots}
import org.scalacheck.{Arbitrary, Gen}
import test.rdts.DataGenerator.*

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given LocalReplicaId                  = "me".asId

    assert(!flag.read)
    val enabled = flag.enable()
    assert(enabled.read)
    val disabled = enabled.disable()
    assert(!disabled.read)

  }

}
