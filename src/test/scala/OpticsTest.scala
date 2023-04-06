package lore
import lore.AST._
import cats.implicits._
import monocle.syntax.all._
import monocle.Lens
import munit.FunSuite
import monocle.macros.GenLens
import cats.parse.Caret
import lore.AST.TSource.sourcePosStart

class OpticsSuite extends FunSuite:

  test("Playground") {
    val a = TSource(TNum(0))
    a.focus(_.sourcePosEnd).replace(None)
    val sp = Lens[Term, Option[Caret]](_.sourcePosStart)(c =>
      t => t.copy(sourcePosStart = c)
    )
    val sourcePos = GenLens[Term](_.sourcePosStart)

  }
