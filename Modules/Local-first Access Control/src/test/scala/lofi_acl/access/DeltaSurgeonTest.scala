package lofi_acl.access

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.DeltaSurgeon.getLabels
import lofi_acl.access.DeltaSurgeonTest.{optionSurgeon, given}
import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.PermissionTree.{allow, empty}
import munit.FunSuite
import org.junit.Assert
import rdts.base
import rdts.base.Bottom

import scala.compiletime.{constValue, constValueTuple, erasedValue, summonAll}

case class A(a: String, b: B)
case class B(c: String)

object DeltaSurgeonTest {
  import DeltaSurgeon.given
  given Bottom[A]       = Bottom.provide(A("", B("")))
  given Bottom[B]       = Bottom.provide(B(""))
  given DeltaSurgeon[B] = DeltaSurgeon.derived
  given DeltaSurgeon[A] = DeltaSurgeon.derived

  given stringCodec: JsonValueCodec[String]  = JsonCodecMaker.make
  private given stringBottom: Bottom[String] = Bottom.provide("")
  given DeltaSurgeon[String]                 = DeltaSurgeon.ofTerminalValue
  private val optionSurgeon                  = DeltaSurgeon.optionSurgeon[String]
}

class DeltaSurgeonTest extends FunSuite {
  test("isolate") {
    val isolated = DeltaSurgeon[A].isolate(A("a string", B("b string")))

    Assert.assertArrayEquals(
      writeToArray("a string"),
      isolated.inner.asInstanceOf[Map[String, IsolatedDeltaParts]]("a").inner.asInstanceOf[Array[Byte]]
    )

    Assert.assertArrayEquals(
      writeToArray("b string"),
      isolated.inner.asInstanceOf[Map[String, IsolatedDeltaParts]]("b")
        .inner.asInstanceOf[Map[String, IsolatedDeltaParts]]("c")
        .inner.asInstanceOf[Array[Byte]]
    )
  }

  test("isolated only contains non-bottom values") {
    assert(DeltaSurgeon[A].isolate(A("", B(""))).isEmpty)
    val isolated = DeltaSurgeon[A].isolate(A("", B("non-bottom")))
    assertEquals(isolated.inner.asInstanceOf[Map[String, IsolatedDeltaParts]].keySet, Set("b"))
    assertEquals(
      readFromArray(isolated.inner.asInstanceOf[Map[String, IsolatedDeltaParts]]("b")
        .inner.asInstanceOf[Map[String, IsolatedDeltaParts]]("c")
        .inner.asInstanceOf[Array[Byte]]),
      "non-bottom"
    )
  }

  test("recombine restores delta from isolated delta parts") {
    Seq(
      A("Test", B("1")),
      A("Test", B("")),
      A("", B("1")),
      A("", B("")),
    ).foreach { a =>
      assertEquals(DeltaSurgeon[A].recombine(DeltaSurgeon[A].isolate(a)), a)
    }
  }

  test("filter") {
    val isolated = DeltaSurgeon[A].isolate(A("Test", B("Test B")))
    assertEquals(
      DeltaSurgeon[A].recombine(DeltaSurgeon[A].filter(isolated, PermissionTree(ALLOW, Map.empty))),
      A("Test", B("Test B"))
    )

    assertEquals(
      DeltaSurgeon[A].recombine(DeltaSurgeon[A].filter(isolated, PermissionTree(PARTIAL, Map("a" -> allow)))),
      A("Test", B(""))
    )

    assertEquals(
      DeltaSurgeon[A].recombine(DeltaSurgeon[A].filter(isolated, PermissionTree(PARTIAL, Map("b" -> allow)))),
      A("", B("Test B"))
    )

    assertEquals(
      DeltaSurgeon[A].recombine(DeltaSurgeon[A].filter(isolated, empty)),
      A("", B(""))
    )
  }

  test("derivation of sums") {
    val some: Option[String] = Some("Test")
    val none: Option[String] = None

    assertEquals(
      optionSurgeon.recombine(optionSurgeon.isolate(some)),
      some
    )
    assertEquals(
      optionSurgeon.recombine(optionSurgeon.isolate(none)),
      none
    )
  }
}
