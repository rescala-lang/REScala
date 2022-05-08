package test.kofre

import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext.*
import kofre.contextual.{AsCausalContext, WithContext}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import test.kofre.DataGenerator.*
import kofre.base.Lattice.Operators
import kofre.base.Lattice
import kofre.decompose.interfaces.EnableWinsFlag
import kofre.syntax.WithNamedContext

class SyntaxTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "Manual Tests" in {

    val flag: WithNamedContext[EnableWinsFlag] = WithNamedContext.empty("me")

    assert(!flag.read)
    val enabled = flag.enable()
    assert(enabled.read)
    val disabled = enabled.disable()
    assert(!disabled.read)

  }

}
