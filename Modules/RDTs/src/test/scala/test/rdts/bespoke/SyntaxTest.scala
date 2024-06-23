package test.rdts.bespoke

import org.scalacheck.{Arbitrary, Gen}
import rdts.base.{Lattice, LocalUid}
import rdts.base.Uid.asId
import rdts.datatypes.contextual.EnableWinsFlag
import rdts.dotted.HasDots.*
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}
import test.rdts.DataGenerator.*

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given LocalUid                   = "me".asId

    assert(!flag.data.read)
    val enabled = flag.mod(_.enable())
    assert(enabled.data.read)
    val disabled = enabled.mod(_.disable())
    assert(!disabled.data.read)

  }

}
