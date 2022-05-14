package test.kofre

import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.time.{Dots, Dot}
import kofre.contextual.HasDots.*
import kofre.contextual.{HasDots, Dotted}
import kofre.datatypes.EnableWinsFlag
import kofre.syntax.DottedName
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.*

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: DottedName[EnableWinsFlag] = DottedName.empty("me")

    assert(!flag.read)
    val enabled = flag.enable()
    assert(enabled.read)
    val disabled = enabled.disable()
    assert(!disabled.read)

  }

}
