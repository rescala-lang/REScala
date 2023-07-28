package test.kofre

import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.base.Uid.asId
import kofre.datatypes.contextual.EnableWinsFlag
import kofre.dotted.HasDots.*
import kofre.dotted.{Dotted, HasDots}
import kofre.syntax.ReplicaId
import kofre.time.{Dot, Dots}
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.*

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given ReplicaId                  = "me".asId

    assert(!flag.read)
    val enabled = flag.enable()
    assert(enabled.read)
    val disabled = enabled.disable()
    assert(!disabled.read)

  }

}
