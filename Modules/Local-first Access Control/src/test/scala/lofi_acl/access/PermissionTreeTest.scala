package lofi_acl.access

import lofi_acl.access.Permission.PARTIAL
import lofi_acl.access.PermissionTree.{allow, empty}
import munit.FunSuite
import rdts.base.Bottom

class PermissionTreeTest extends FunSuite {
  def partial(labelsWithTree: (String, PermissionTree)*): PermissionTree =
    PermissionTree(PARTIAL, labelsWithTree.toMap)

  test("fromPath base cases") {
    assertEquals(
      PermissionTree.fromPath(""),
      allow
    )

    assertEquals(
      PermissionTree.fromPath("a"),
      PermissionTree(PARTIAL, Map("a" -> allow))
    )

    assertEquals(
      PermissionTree.fromPath("a.b"),
      PermissionTree(PARTIAL, Map("a" -> PermissionTree(PARTIAL, Map("b" -> allow))))
    )
  }

  test("fromPath fails with empty path element") {
    intercept[IllegalArgumentException](
      PermissionTree.fromPath(".")
    )

    intercept[IllegalArgumentException](
      PermissionTree.fromPath("..")
    )

    intercept[IllegalArgumentException](
      PermissionTree.fromPath("...")
    )

    intercept[IllegalArgumentException](
      PermissionTree.fromPath("a..")
    )

    intercept[IllegalArgumentException](
      PermissionTree.fromPath("..b")
    )

    intercept[IllegalArgumentException](
      PermissionTree.fromPath("a..b")
    )
  }

  test("fromPath with *") {
    assertEquals(
      PermissionTree.fromPath("*"),
      allow
    )

    assertEquals(
      PermissionTree.fromPath("a.*"),
      PermissionTree(PARTIAL, Map("a" -> allow))
    )

    assertEquals(
      PermissionTree.fromPath("a.*.b.*"),
      partial("a" -> partial("*" -> partial("b" -> allow)))
    )
  }

  test("fromPath with *.*") {
    assertEquals(
      PermissionTree.fromPath("*.*"),
      allow
    )

    assertEquals(
      PermissionTree.fromPath("a.*.*"),
      partial("a" -> allow)
    )

    assertEquals(
      PermissionTree.fromPath("a.*.*.b"),
      partial("a" -> partial("*" -> partial("*" -> partial("b" -> allow))))
    )

    assertEquals(
      PermissionTree.fromPath("*.*.b"),
      partial("*" -> partial("*" -> partial("b" -> allow)))
    )
  }

  test("merge with *") {
    assertEquals(
      PermissionTree.lattice.merge(
        partial("*" -> partial("a" -> allow)),
        partial("*" -> partial("b" -> allow))
      ),
      partial("*" -> partial("a" -> allow, "b" -> allow))
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("a" -> partial("b" -> partial("c" -> allow))),
        partial("a" -> partial("*" -> partial("d" -> allow))),
      ),
      partial("a" -> partial(
        "b" -> partial("c" -> allow, "d" -> allow),
        "*" -> partial("d" -> allow)
      ))
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("a" -> partial("b" -> partial("c" -> allow))),
        partial("*" -> partial("*" -> partial("d" -> allow))),
      ),
      partial(
        "a" -> partial(
          "b" -> partial("c" -> allow, "d" -> allow),
          "*" -> partial("d" -> allow)
        ),
        "*" -> partial("*" -> partial("d" -> allow))
      )
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("*" -> partial("a" -> partial("b" -> allow))),
        partial("*" -> partial("*" -> partial("c" -> allow)))
      ),
      partial("*" -> partial(
        "a" -> partial("b" -> allow, "c" -> allow),
        "*" -> partial("c" -> allow)
      ))
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("*" -> partial("*" -> partial("a" -> allow))),
        partial("*" -> partial("*" -> partial("b" -> allow)))
      ),
      partial("*" -> partial("*" -> partial("a" -> allow, "b" -> allow))),
    )
  }

  test("normalization") {
    assertEquals(
      PermissionTree.lattice.merge(
        partial("*" -> partial()),
        partial("*" -> partial())
      ),
      partial()
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("*" -> allow),
        partial("*" -> partial())
      ),
      allow
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("a" -> allow),
        partial("*" -> partial())
      ),
      partial("a" -> allow)
    )
  }

  test("bottom") {
    assert(Bottom[PermissionTree].empty == PermissionTree(PARTIAL, Map.empty))
    assert(partial().isEmpty)
  }

  test("merge without *") {
    assertEquals(
      PermissionTree.lattice.merge(
        partial("a" -> partial("b" -> allow)),
        partial("a" -> partial("c" -> allow)),
      ),
      partial("a" -> partial("b" -> allow, "c" -> allow))
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial("a" -> partial("b" -> partial("c" -> allow))),
        partial("a" -> partial("b" -> allow)),
      ),
      partial("a" -> partial("b" -> allow))
    )

    assertEquals(
      PermissionTree.lattice.merge(
        partial(),
        partial()
      ),
      partial()
    )

    assertEquals(
      PermissionTree.lattice.merge(
        empty,
        allow
      ),
      allow
    )
  }

  test("merge normalizes tree") {
    assertEquals(
      PermissionTree.lattice.merge(
        partial("a" -> partial("b" -> partial())),
        partial("c" -> partial())
      ),
      partial()
    )
  }

  test("lattice properties") {
    ???
  }

  test("Normalized trees only have allow as leafs or are empty") {
    ???
  }

  test("Non-wildcard siblings of wildcards are at least as permissive as wildcard sibling") {
    ???
  }
}
