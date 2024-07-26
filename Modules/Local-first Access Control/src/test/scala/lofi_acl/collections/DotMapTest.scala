package lofi_acl.collections

import munit.FunSuite
import rdts.base.Uid
import rdts.time.Dot

class DotMapTest extends FunSuite {
  private val a = Uid("a")
  private val b = Uid("b")
  private val c = Uid("c")

  test("from and iterator") {
    List(
      List(),
      List(Dot(a, 0) -> "a0"),
      List(Dot(a, 0) -> "a0", Dot(b, 0)  -> "b0", Dot(c, 0)  -> "c0"),
      List(Dot(b, 1) -> "b1"),
      List(Dot(a, 0) -> "a0", Dot(b, 42) -> "b42", Dot(c, 0) -> "c0"),
      List(
        Dot(a, 0)  -> "a0",
        Dot(a, 1)  -> "a1",
        Dot(a, 1)  -> "a1",
        Dot(b, 10) -> "b10",
        Dot(b, 42) -> "b42",
        Dot(c, 0)  -> "c0"
      ),
    ).foreach { list =>
      assertEquals(list, list.iterator.toList)
    }
  }

  test("removed") {
    DotMap.empty[String].removed(Dot(a, 0)).isEmpty

    {
      val map = DotMap.from(List(Dot(a, 42) -> "a42"))
      assertEquals(map.removed(Dot(a, 0)).iterator.toList, List(Dot(a, 42) -> "a42"))
      assertEquals(map.removed(Dot(a, 0)).iterator.toList, List(Dot(a, 42) -> "a42"))
    }

    val entries = Map(
      Dot(a, 0)  -> "a0",
      Dot(a, 1)  -> "a1",
      Dot(a, 1)  -> "a1",
      Dot(b, 10) -> "b10",
      Dot(b, 42) -> "b42",
      Dot(c, 0)  -> "c0"
    )

    // Remove every entry, but always from full map
    val dotMap = DotMap.from(entries)
    entries.foreach { (dot, value) =>
      val updatedMap  = dotMap.removed(dot)
      val expectedMap = entries.removed(dot)

      assert(!updatedMap.contains(dot))
      assertEquals(updatedMap.toSet.toMap, expectedMap)
      assertEquals(updatedMap, DotMap.from(expectedMap)) // Also test DotMap::equals(other) and from
      assertEquals(updatedMap.removed(dot).toSet.toMap, expectedMap)
    }

    // Remove every entry, but use result of previous removal starting with full map
    val entryIterator = entries.iterator
    entryIterator.foldLeft(dotMap) { (dotMap, entry) =>
      val withoutEntry = dotMap.removed(entry._1)
      assertEquals(withoutEntry.toList.toMap, dotMap.toList.toMap.removed(entry._1))
      withoutEntry
    }
  }

  test("updated") {
    val entries = Map(
      Dot(a, 0)  -> "a0",
      Dot(a, 1)  -> "a1",
      Dot(a, 1)  -> "a1",
      Dot(b, 10) -> "b10",
      Dot(b, 42) -> "b42",
      Dot(c, 0)  -> "c0"
    )

    // Add every entry, but always to empty Map
    entries.foreach { (dot, value) =>
      val updatedMap  = DotMap.empty[String].updated(dot, value)
      val expectedMap = Map(dot -> value)

      assert(updatedMap.contains(dot))
      assertEquals(updatedMap.toSet.toMap, expectedMap)
      assertEquals(updatedMap, DotMap.from(expectedMap)) // Also test DotMap::equals(other) and from
      assertEquals(updatedMap.updated(dot, "something else").toSet.toMap, Map(dot -> "something else"))
    }

    // Update every entry, but use result of previous removal starting with full map
    val entryIterator = entries.iterator
    entryIterator.foldLeft(DotMap.from(entries)) { (dotMap, entry) =>
      val updated = dotMap.updated(entry._1, "something else")
      assertEquals(updated.toList.toMap, dotMap.toList.toMap.updated(entry._1, "something else"))
      updated
    }
  }

  test("get") {
    List(
      List(),
      List(Dot(a, 0) -> "a0"),
      List(Dot(a, 0) -> "a0", Dot(b, 0)  -> "b0", Dot(c, 0)  -> "c0"),
      List(Dot(b, 1) -> "b1"),
      List(Dot(a, 0) -> "a0", Dot(b, 42) -> "b42", Dot(c, 0) -> "c0"),
      List(
        Dot(a, 0)  -> "a0",
        Dot(a, 1)  -> "a1",
        Dot(a, 3)  -> "a3",
        Dot(b, 10) -> "b10",
        Dot(b, 42) -> "b42",
        Dot(c, 0)  -> "c0"
      ),
    ).foreach { list =>
      val map = DotMap.from(list)
      list.foreach(elem => assertEquals(map.get(elem._1), Some(elem._2)))
    }

    val map = DotMap.from(List(
      Dot(a, 0)  -> "a0",
      Dot(a, 1)  -> "a1",
      Dot(a, 3)  -> "a3",
      Dot(b, 10) -> "b10",
      Dot(b, 42) -> "b42",
      Dot(c, 0)  -> "c0"
    ))

    assertEquals(map.get(Dot(a, 2)), None)
    assertEquals(map.get(Dot(a, 4)), None)
    assertEquals(map.get(Dot(b, 0)), None)
    assertEquals(map.get(Dot(c, 42)), None)
    assertEquals(map.get(Dot(Uid("d"), 0)), None)
  }
}
