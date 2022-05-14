package test.kofre

import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.time.{Dots, Dot}
import kofre.contextual.AsCausalContext.*
import kofre.contextual.{AsCausalContext, WithContext}
import kofre.datatypes.EnableWinsFlag
import kofre.syntax.WithNamedContext
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.*

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: WithNamedContext[EnableWinsFlag] = WithNamedContext.empty("me")

    assert(!flag.read)
    val enabled = flag.enable()
    assert(enabled.read)
    val disabled = enabled.disable()
    assert(!disabled.read)

  }

}
