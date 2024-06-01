import com.softwaremill.quicklens.*
import rdts.base.Bottom

class BasicTest extends munit.FunSuite {


  given Bottom[Int] = Bottom.provide(42)
  given Bottom[String] = Bottom.provide("bottom")

  case class TestClass(str: String, int: Int) derives Bottom




  test("basics") {

    val a = TestClass("test", 11)

    val res = a.modify(_.str).using(_.toUpperCase)

    assertEquals(res, TestClass("TEST", 42))


  }

}
