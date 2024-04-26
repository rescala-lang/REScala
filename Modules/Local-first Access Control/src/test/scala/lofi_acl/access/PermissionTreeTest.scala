package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, DENY}
import lofi_acl.access.PermissionTree.{allow, deny}
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

  def pt(permission: Permission, children: (String, PermissionTree)*): PermissionTree =
    PermissionTree(permission, children.toMap)

  def testAllPermutations(list: List[(String, Permission)], expectedResult: PermissionTree): Unit = {
    list.permutations.foreach { list =>
      assertEquals(list.toPermissionTree, expectedResult)
    }
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
    intercept[IllegalArgumentException](
      List("a..b" -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("a...b" -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("..b" -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("b.." -> ALLOW).toPermissionTree
    )
  }

  test("a..b -> ALLOW") {
    intercept[IllegalArgumentException](
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
    testAllPermutations(
      List("b1.a" -> ALLOW, "b2" -> ALLOW),
      PermissionTree(
        DENY,
        Map(
          "b1" -> PermissionTree(DENY, Map("a" -> PermissionTree(ALLOW, Map.empty))),
          "b2" -> PermissionTree(ALLOW, Map.empty)
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
    testAllPermutations(
      List("a.b.c" -> ALLOW, "a.b.e" -> ALLOW),
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
    testAllPermutations(
      List("a"       -> ALLOW, "a.b.c" -> ALLOW, "a.b" -> DENY),
      denyExcept("a" -> allowExcept("b" -> denyExcept("c" -> allow)))
    )
  }

  // ------------------ Pruning ---------------------------
  test("a -> ALLOW & a.b -> ALLOW & a.c -> ALLOW") {
    assertEquals(
      List("a" -> ALLOW, "a.b" -> ALLOW, "a.c" -> ALLOW).toPermissionTree,
      denyExcept("a" -> allow)
    )
  }

  test("a & a.b.c.d") {
    testAllPermutations(
      List(
        "a"       -> ALLOW,
        "a.b.c.d" -> ALLOW
      ),
      denyExcept("a" -> allow)
    )

    testAllPermutations(
      List(
        "a"       -> ALLOW,
        "a.b.c.d" -> DENY
      ),
      denyExcept("a" -> allowExcept("b" -> allowExcept("c" -> allowExcept("d" -> deny))))
    )

    testAllPermutations(
      List(
        ""        -> ALLOW,
        "a"       -> DENY,
        "a.b.c.d" -> DENY
      ),
      allowExcept("a" -> deny)
    )
  }

  test("a.b.c -> ALLOW & a.b.e -> DENY") {
    // a.b.e -> DENY should be pruned
    testAllPermutations(
      List("a.b.c" -> ALLOW, "a.b.e" -> DENY),
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(DENY, Map("b" -> PermissionTree(DENY, Map("c" -> PermissionTree(ALLOW, Map.empty))))))
      )
    )
  }

  test("a -> ALLOW & a.b -> ALLOW & a.b.c -> DENY merges into a -> ALLOW and a.b.c -> DENY") {
    // Check whether a & a.b.c is correct
    testAllPermutations(
      List("a" -> ALLOW, "a.b.c" -> DENY),
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

  test("Trailing .*") {
    assertEquals(
      List("a.*" -> ALLOW).toPermissionTree,
      PermissionTree(DENY, Map("a" -> allow))
    )

    assertEquals(
      List("a.b.c.*" -> ALLOW).toPermissionTree,
      denyExcept("a" -> denyExcept("b" -> denyExcept("c" -> allow)))
    )

    testAllPermutations(
      List(
        "a.*"   -> ALLOW,
        "b"     -> ALLOW,
        "b.c.*" -> DENY,
        "c"     -> ALLOW
      ),
      denyExcept("a" -> allow, "b" -> allowExcept("c" -> deny), "c" -> allow)
    )
  }

  test(".* as prefix should fail") {
    intercept[IllegalArgumentException](
      List("a" -> ALLOW, ".*" -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List("a" -> ALLOW, ".*" -> ALLOW).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List(".*" -> DENY).toPermissionTree
    )

    intercept[IllegalArgumentException](
      List(".*.b" -> ALLOW).toPermissionTree
    )
  }

  test("** should fail") {
    List("**" -> ALLOW, "a" -> ALLOW, "b" -> ALLOW).permutations.foreach { list =>
      intercept[IllegalArgumentException](list.toPermissionTree)
    }

    intercept[IllegalArgumentException](List("**" -> DENY).toPermissionTree)
    intercept[IllegalArgumentException](List("a.**" -> DENY).toPermissionTree)
    intercept[IllegalArgumentException](List("a.**.b" -> ALLOW).toPermissionTree)
  }

  test("a.* & a should fail") {
    intercept[IllegalArgumentException](List("a.*" -> ALLOW, "a" -> ALLOW).toPermissionTree)
    intercept[IllegalArgumentException](List("a" -> ALLOW, "a.*" -> ALLOW).toPermissionTree)
    intercept[IllegalArgumentException](List("a.*" -> ALLOW, "a" -> DENY).toPermissionTree)
    intercept[IllegalArgumentException](List("a" -> DENY, "a.*" -> ALLOW).toPermissionTree)
  }

  test("a.*.*.*") {
    assertEquals(
      List("a.*.*.*" -> ALLOW).toPermissionTree,
      denyExcept("a" -> denyExcept("*" -> denyExcept("*" -> allow)))
    )
  }

  test("a -> ALLOW & a.b.c -> ALLOW & a.b.* -> DENY") {
    testAllPermutations(
      List("a" -> ALLOW, "a.b.c" -> ALLOW, "a.b.*" -> DENY),
      PermissionTree(DENY, Map("a" -> PermissionTree(ALLOW, Map("b" -> PermissionTree(DENY, Map("c" -> allow))))))
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

  test("a.* -> ALLOW & a.*.c -> DENY") {
    testAllPermutations(
      List("a.*" -> ALLOW, "a.*.c" -> DENY),
      PermissionTree(DENY, Map("a" -> PermissionTree(ALLOW, Map("*" -> PermissionTree(ALLOW, Map("c" -> deny))))))
    )
  }

  test("a.*.c -> ALLOW & a.b.c -> DENY") {
    // Note: a.b.c is more specific than a.*.c
    testAllPermutations(
      List("a.*.c" -> ALLOW, "a.b.c" -> DENY),
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(
          DENY,
          Map(
            "*" -> PermissionTree(DENY, Map("c" -> PermissionTree(ALLOW, Map.empty))),
            "b" -> PermissionTree(DENY, Map("c" -> PermissionTree(DENY, Map.empty)))
          )
        ))
      )
    )
  }

  test("a.*.c -> DENY & a.b.c -> ALLOW") {
    // Note: a.b.c is more specific than a.*.c
    testAllPermutations(
      List("a.*.c" -> DENY, "a.b.c" -> ALLOW),
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(
          DENY,
          Map(
            "*" -> deny,
            "b" -> PermissionTree(DENY, Map("c" -> PermissionTree(ALLOW, Map.empty)))
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

  // ------------- WILDCARD RULE DUPLICATION -------------
  test("a.*.c -> DENY & a.b -> ALLOW") {
    testAllPermutations(
      List("a.*.c" -> DENY, "a.b" -> ALLOW),
      PermissionTree(
        DENY,
        Map("a" -> PermissionTree(
          DENY,
          Map(
            // "*" -> PermissionTree(DENY, Map("c" -> DENY)), <--- Pruned
            "b" -> PermissionTree(ALLOW, Map("c" -> deny)) // <--- *.c -> deny rule was copied into concrete rule
          )
        ))
      )
    )
  }

  test("a.*.c.* -> ALLOW & a.b.*.d -> DENY & *.b.*.d -> DENY") {
    // Before duplication and pruning
    // pt(
    //  DENY,
    //  "a" -> pt(DENY, "*" -> pt(DENY, "c" -> allow), "b" -> pt(DENY, "*" -> pt(DENY, "d" -> deny))),
    //  "*" -> pt("b" -> pt("*" -> "d" -> deny))
    // )

    testAllPermutations(
      List("a.*.c.*" -> ALLOW, "a.b.*.d" -> DENY, "*.b.*.d" -> DENY),
      pt(DENY, "a"   -> pt(DENY, "*" -> pt(DENY, "c" -> allow)))
    )
  }

  test(" -> ALLOW & a.*.c.d -> ALLOW & a.b.*.d.e -> DENY & *.b.*.d.f -> DENY") {
    testAllPermutations(
      List("" -> ALLOW, "a.*.c.d" -> DENY, "a.b.*.d.e" -> DENY, "*.b.*.d.x" -> DENY),
      pt (
        ALLOW,
        "a" -> pt(
          ALLOW,
          "*" -> pt(ALLOW, "c" -> pt(ALLOW, "d" -> deny)),
          "b" -> pt(ALLOW, "*" -> pt(ALLOW, "d" -> pt(ALLOW, "e" -> deny, "x" -> deny)))
        ),
        "*" -> pt(ALLOW, "b" -> pt(ALLOW, "*" -> pt(ALLOW, "d" -> pt(ALLOW, "x" -> deny))))
      )
    )
  }

  test(" -> ALLOW & a.* -> DENY & a.b.* -> ALLOW & a.b.c.* -> DENY") {
    testAllPermutations(
      List(""       -> ALLOW, "a.*" -> DENY, "a.b.*" -> ALLOW, "a.b.c.*" -> DENY),
      pt(ALLOW, "a" -> pt(DENY, "b" -> pt(ALLOW, "c" -> deny)))
    )
  }

  test("-> ALLOW & a.*.c -> DENY & a.b.c.d -> ALLOW") {
    // a.b.c.d should be allowed, since it's more specific
    testAllPermutations(
      List(
        ""        -> ALLOW,
        "a.*.c"   -> DENY,
        "a.b.c.d" -> ALLOW
      ),
      pt(ALLOW, "a" -> pt(ALLOW, "*" -> pt(ALLOW, "c" -> deny), "b" -> pt(ALLOW, "c" -> pt(DENY, "d" -> allow))))
    )
  }

  test("-> ALLOW & a.*.c -> DENY & a.b.c -> ALLOW") {
    testAllPermutations(
      List(
        ""        -> ALLOW,
        "a.*.c"   -> DENY,
        "a.b.c.d" -> ALLOW
      ),
      pt(ALLOW, "a" -> pt(ALLOW, "*" -> pt(ALLOW, "c" -> deny), "b" -> pt(ALLOW, "c" -> pt(DENY, "d" -> allow))))
    )
  }

  test("a.b.c.d -> ALLOW & a.*.x -> ALLOW & a.*.*.d -> DENY") {
    testAllPermutations(
      List("a.b.c.d" -> ALLOW, "a.*.x" -> ALLOW, "a.*.*.d" -> DENY),
      pt(
        DENY,
        "a" -> pt(
          DENY,
          "b" -> pt(DENY, "c" -> pt(DENY, "d" -> allow)),
          "*" -> pt(DENY, "x" -> pt(ALLOW, "d" -> deny)),
        )
      )
    )
  }
}
