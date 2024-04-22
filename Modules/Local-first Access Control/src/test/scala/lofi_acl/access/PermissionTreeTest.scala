package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, DENY}
import lofi_acl.access.PermissionTree.allow
import lofi_acl.access.PermissionTreeValidationException.InvalidPathException
import munit.FunSuite

class PermissionTreeTest extends FunSuite {
  extension (acl: List[(String, Permission)])
    def toPermissionTree: PermissionTree = PermissionTree.fromPathList(acl)

  def denyExcept(labelsWithTree: (String, PermissionTree)*): PermissionTree = {
    PermissionTree(DENY, labelsWithTree.toMap)
  }

  def allowExcept(labelsWithTree: (String, PermissionTree)*): PermissionTree = {
    PermissionTree(ALLOW, labelsWithTree.toMap)
  }

  test("PermissionTree.deny and PermissionTree.allow") {
    assertEquals(PermissionTree.deny, PermissionTree(DENY, Map.empty))
    assertEquals(PermissionTree.allow, PermissionTree(ALLOW, Map.empty))
  }

  test("\"\" -> ALLOW") {
    assertEquals(
      List("" -> ALLOW).toPermissionTree,
      PermissionTree(ALLOW, Map.empty)
    )
  }

  test("trailing . should not parse") {
    intercept[IllegalArgumentException](
      List("test." -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("." -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("a.b.c.d.e." -> ALLOW).toPermissionTree
    )
  }

  test("empty label between separator") {
    intercept[InvalidPathException](
      List("a..b" -> ALLOW).toPermissionTree
    )

    intercept[InvalidPathException](
      List("a...b" -> ALLOW).toPermissionTree
    )

    intercept[InvalidPathException](
      List("..b" -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("b.." -> ALLOW).toPermissionTree
    )
  }

  test("a..b -> ALLOW") {
    intercept[InvalidPathException](
      List("a..b" -> ALLOW).toPermissionTree
    )
  }

  test("b1 -> ALLOW") {
    assertEquals(
      List("b1" -> ALLOW).toPermissionTree,
      PermissionTree(DENY, Map("b1" -> PermissionTree(ALLOW, Map.empty)))
    )
  }

  test("b1 -> ALLOW & b2 -> ALLOW") {
    val expectedPermissionTree =
      PermissionTree(DENY, Map("b1" -> PermissionTree(ALLOW, Map.empty), "b2" -> PermissionTree(ALLOW, Map.empty)))

    assertEquals(
      List("b1" -> ALLOW, "b2" -> ALLOW).toPermissionTree,
      expectedPermissionTree
    )

    // Order doesn't matter
    assertEquals(
      List("b2" -> ALLOW, "b1" -> ALLOW).toPermissionTree,
      expectedPermissionTree
    )
  }

  test("b1.a -> ALLOW & b2 -> ALLOW") {
    assertEquals(
      List("b1.a" -> ALLOW, "b2" -> ALLOW).toPermissionTree,
      PermissionTree(
        DENY,
        Map(
          "b1" -> PermissionTree(DENY, Map("a" -> PermissionTree(ALLOW, Map.empty))),
          "b2" -> PermissionTree(ALLOW, Map.empty)
        )
      )
    )

    // Order of rules doesn't matter
    assertEquals(
      List("b2" -> ALLOW, "b1.a" -> ALLOW).toPermissionTree,
      PermissionTree(
        DENY,
        Map(
          "b2" -> PermissionTree(ALLOW, Map.empty),
          "b1" -> PermissionTree(DENY, Map("a" -> PermissionTree(ALLOW, Map.empty)))
        )
      )
    )
  }

  test("a.b.c.d.e.f.g") {
    assertEquals(
      List("a.b.c.d.e.f.g" -> DENY).toPermissionTree,
      PermissionTree.deny
    )

    assertEquals(
      List("a.b.c.d.e.f.g" -> ALLOW).toPermissionTree,
      denyExcept("a" -> denyExcept(
        "b" -> denyExcept("c" -> denyExcept("d" -> denyExcept("e" -> denyExcept("f" -> denyExcept("g" -> allow)))))
      ))
    )
  }

  test("a.b.c -> ALLOW & a.b.e -> ALLOW") {
    assertEquals(
      List("a.b.c" -> ALLOW, "a.b.e" -> ALLOW).toPermissionTree,
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(
          DENY,
          Map(
            "b" -> PermissionTree(
              DENY,
              Map("c" -> PermissionTree.allow, "e" -> PermissionTree.allow)
            )
          )
        ))
      )
    )
  }

  test("a -> ALLOW & a.b.c -> ALLOW & a.b -> DENY") {
    assertEquals(
      List("a" -> ALLOW, "a.b.c" -> ALLOW, "a.b" -> DENY).toPermissionTree,
      denyExcept("a" -> allowExcept("b" -> denyExcept("c" -> allow)))
    )
  }

  test("a.b.c -> ALLOW & a.b.e -> DENY") {
    // a.b.e -> DENY should be pruned
    assertEquals(
      List("a.b.c" -> ALLOW, "a.b.e" -> DENY).toPermissionTree,
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(DENY, Map("b" -> PermissionTree(DENY, Map("c" -> PermissionTree(ALLOW, Map.empty))))))
      )
    )

    // Regardless of order
    assertEquals(
      List("a.b.e" -> DENY, "a.b.c" -> ALLOW).toPermissionTree,
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(DENY, Map("b" -> PermissionTree(DENY, Map("c" -> PermissionTree(ALLOW, Map.empty))))))
      )
    )
  }

  test("a -> ALLOW & a.b -> ALLOW & a.b.c -> DENY merges into a -> ALLOW and a.b.c -> DENY") {
    // Check whether a & a.b.c is correct
    assertEquals(
      List("a" -> ALLOW, "a.b.c" -> DENY).toPermissionTree,
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(ALLOW, Map("b" -> PermissionTree(ALLOW, Map("c" -> PermissionTree.deny)))))
      )
    )

    // Now check whether the more-specific but duplicate rule is merged
    assertEquals(
      List("a" -> ALLOW, "a.b" -> ALLOW, "a.b.c" -> DENY).toPermissionTree,
      List("a" -> ALLOW, "a.b.c" -> DENY).toPermissionTree
    )
  }

  // ----------- WILDCARDS ----------------

  test("b1.* -> ALLOW") {
    assertEquals(
      List("b1" -> ALLOW).toPermissionTree,
      PermissionTree(DENY, Map("b1" -> PermissionTree(ALLOW, Map.empty)))
    )
  }

  test("b1.*.e -> ALLOW") {
    assertEquals(
      List("b1.*.e" -> ALLOW).toPermissionTree,
      PermissionTree(
        DENY,
        Map("b1" -> PermissionTree(
          DENY,
          Map("*" -> PermissionTree(DENY, Map("e" -> PermissionTree.allow)))
        ))
      )
    )
  }

  test("b1.*.e -> ALLOW & b1.d.e -> DENY") {
    assertEquals(
      List("b1.*.e" -> ALLOW, "b1.d.e" -> DENY).toPermissionTree,
      PermissionTree(
        DENY,
        Map("b1" -> PermissionTree(
          DENY,
          Map(
            "*" -> PermissionTree(DENY, Map("e" -> PermissionTree(ALLOW, Map.empty))),
            "e" -> PermissionTree(DENY, Map("e" -> PermissionTree(DENY, Map.empty)))
          )
        ))
      )
    )
  }

  test("Passing both b1.* -> ALLOW and b1.* -> DENY should fail") {
    intercept[IllegalArgumentException](
      List("b1.*" -> ALLOW, "b1.*" -> DENY).toPermissionTree
    )
  }
}
