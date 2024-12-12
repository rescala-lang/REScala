package lofi_acl.access

import lofi_acl.access.Permission.PARTIAL
import lofi_acl.access.PermissionTree.allow
import lofi_acl.ardt.datatypes.LWW.given
import munit.FunSuite
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins

given stringBottom: Bottom[String] = Bottom.provide("")

case class TestEmpty() derives Bottom

case class TestSingle(a: LastWriterWins[String]) derives Bottom

case class TestNested(a: TestSingle, b: Option[LastWriterWins[String]]) derives Bottom

case class TestForWildcard(a: TestNested, b: TestNested) derives Bottom

class FilterTest extends FunSuite {
  test("ProductTypeFilter with empty product") {
    given filter: Filter[TestEmpty] = Filter.derived
    assertEquals(filter.filter(TestEmpty(), allow), TestEmpty())
    assertEquals(filter.filter(TestEmpty(), PermissionTree.empty), TestEmpty())
  }

  test("ProductTypeFilter with simple product") {
    given filter: Filter[TestSingle] = Filter.derived
    val testValue                    = TestSingle(LastWriterWins.now("Test"))
    assertEquals(filter.filter(testValue, allow), testValue)
    assertEquals(filter.filter(testValue, PermissionTree.empty), TestSingle(LastWriterWins.empty))
  }

  test("ProductTypeFilter with nested product") {
    given Filter[TestSingle] = Filter.derived
    import lofi_acl.ardt.base.StandardLibrary.OptionLattice.filter
    given filter: Filter[TestNested] = Filter.derived
    val testValue = TestNested(TestSingle(LastWriterWins.now("Test a")), Some(LastWriterWins.now("Test b")))
    assertEquals(filter.filter(testValue, allow), testValue)
    assertEquals(
      filter.filter(testValue, PermissionTree(PARTIAL, Map("a" -> allow, "b" -> allow))),
      testValue
    )
    assertEquals(filter.filter(testValue, PermissionTree.empty), TestNested(TestSingle(LastWriterWins.empty), None))
    assertEquals(
      filter.filter(testValue, PermissionTree(PARTIAL, Map("a" -> allow))),
      TestNested(testValue.a, None)
    )
    assertEquals(
      filter.filter(testValue, PermissionTree(PARTIAL, Map("b" -> allow))),
      TestNested(TestSingle(LastWriterWins.empty), testValue.b)
    )
  }

  test("wildcard with nested product") {
    given Filter[TestSingle] = Filter.derived
    import lofi_acl.ardt.base.StandardLibrary.OptionLattice.filter
    given Filter[TestNested]              = Filter.derived
    given filter: Filter[TestForWildcard] = Filter.derived

    val testValue = TestForWildcard(
      TestNested(TestSingle(LastWriterWins.now("A1")), Some(LastWriterWins.now("B1"))),
      TestNested(TestSingle(LastWriterWins.now("A2")), Some(LastWriterWins.now("B2")))
    )

    assertEquals(
      filter.filter(
        testValue,
        PermissionTree(PARTIAL, Map("*" -> PermissionTree(PARTIAL, Map("a" -> allow))))
      ),
      TestForWildcard(
        testValue.a.copy(b = None),
        testValue.b.copy(b = None)
      )
    )

    test("derivation of sum") {
      given Bottom[Some[String]]           = Bottom.provide(Some(""))
      given Bottom[None.type]              = Bottom.provide(None)
      given Filter[None.type]              = Filter.ofTerminalValue
      given Filter[Some[String]]           = Filter.ofTerminalValue
      given filter: Filter[Option[String]] = Filter.derived

      assertEquals(filter.filter(Some("Hello"), allow), Some("Hello"))
      assertEquals(filter.filter(Some("Hello"), PermissionTree.empty), None)
      assertEquals(filter.filter(Some("Hello"), PermissionTree(PARTIAL, Map("Some" -> allow))), Some("Hello"))
      assertEquals(filter.filter(Some("Hello"), PermissionTree(PARTIAL, Map("Some" -> allow))), None)
      assertEquals(filter.filter(Some("Hello"), PermissionTree(PARTIAL, Map("None$" -> allow))), None)
      assertEquals(filter.filter(None, PermissionTree(PARTIAL, Map("None$" -> allow))), None)
      assertEquals(filter.filter(None, PermissionTree(PARTIAL, Map("Some" -> allow, "None$" -> allow))), None)
      assertEquals(
        filter.filter(Some("Hello"), PermissionTree(PARTIAL, Map("Some" -> allow, "None$" -> allow))),
        Some("Hello")
      )
    }
  }
}
