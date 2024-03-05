package test.rdts.bespoke

import rdts.base.Bottom
import rdts.datatypes.LastWriterWins

class LastWriterWinsTest extends munit.FunSuite {

  given stringBottom: Bottom[String] with {
    override def empty: String = ""
  }

  test("basic write") {
    import LastWriterWins.given

    val lww1 = LastWriterWins.now("Hello World")
    val lww2 = lww1.write("Hello Distributed World")

    assertNotEquals(lww1.timestamp, lww2.timestamp)

    val merged = lww1 merge lww2

    assertEquals(merged.read, "Hello Distributed World")
  }

  test("newer initial value overwrites an earlier initial value") {
    import LastWriterWins.given

    val lww1 = LastWriterWins.now("Hello World")
    val lww2 = LastWriterWins.now("Hello Distributed World")

    assertNotEquals(lww1.timestamp, lww2.timestamp)

    val merged = lww1 merge lww2

    assertEquals(merged.read, "Hello Distributed World")
  }

}
