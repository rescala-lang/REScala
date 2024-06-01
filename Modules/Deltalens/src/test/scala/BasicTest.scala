import com.softwaremill.quicklens.*
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins

class BasicTest extends munit.FunSuite {

  given Bottom[Int]    = Bottom.provide(42)
  given Bottom[String] = Bottom.provide("bottom")

  case class TestClass(str: String, int: Int) derives Bottom
  case class TestNesting(distraction: LastWriterWins[String], test: TestClass) derives Bottom

  test("basics") {

    val a = TestNesting(LastWriterWins.now("example"), TestClass("test", 11))

    val res = a.modify(_.test.str).using(_.toUpperCase)

    assertEquals(res, TestNesting(LastWriterWins.bottom[String].empty, TestClass("TEST", 42)))

  }

}
