package fr
import minitest._
import AST._
import fr.Parser
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse._, ScalaWhitespace._

object InvariantParsing extends SimpleTestSuite {
  test("some hardcoded invariants") {
    def p[_: P] = P(Parser.invariant ~ End)

    parse("@invariant forall a: Int :: a >= 0", p(_)) match {
      case Success(_, index) => ()
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse invariant: $x")
    }

    val longInavariant = """
    @invariant forall d: District :: d in Districts ==>
      size(getNewOrdersPerDistrict(d)) ==
        getNewOrdersPerDistrict(d).map(_.ID).max() - 5
    """

    val longerInavariant = """
    @invariant forall d: District :: d in Districts ==>
      size(getNewOrdersPerDistrict(d)) ==
        getNewOrdersPerDistrict(d).map(_.ID).max() -
        getNewOrdersPerDistrict(d).map(_.ID).min() + 1
    """

    parse(longInavariant, p(_)) match {
      case Success(s, index) => print(s)
      case f: Failure => fail(f.trace().longAggregateMsg)
      case x => fail(s"Failed to parse invariant: $x")
    }

    // parse(longerInavariant, p(_)) match {
    //   case Success(s, index) => print(s"Got $s")
    //   case f: Failure => fail(f.trace().longAggregateMsg)
    //   case x => fail(s"Failed to parse invariant: $x")
    // }
  }
}