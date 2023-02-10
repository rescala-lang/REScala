package test.kofre

import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.time.{Dots, Dot}
import kofre.dotted.HasDots.*
import kofre.datatypes.EnableWinsFlag
import kofre.dotted.{Dotted, HasDots}
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.*
import kofre.syntax.Named
import kofre.syntax.ReplicaId
import kofre.base.Id.asId

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given ReplicaId = "me".asId

    assert(!flag.read)
    val enabled = flag.enable()
    assert(enabled.read)
    val disabled = enabled.disable()
    assert(!disabled.read)

  }

}
