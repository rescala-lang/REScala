package lofi_acl.access

import lofi_acl.access.Operation.{READ, WRITE}
import munit.FunSuite

class RuleTest extends FunSuite {
  test("Rule.toAccessiblePath") {
    val aRead  = Rule(Group("A"), READ, "A.read")
    val aWrite = Rule(Group("A"), WRITE, "A.write")
    val bRead  = Rule(Group("B"), READ, "B.read")
    val bWrite = Rule(Group("B"), WRITE, "B.write")

    val onlyC = Set(Group("C"))

    assert(aRead.toPathIfAllowed(onlyC, READ).isEmpty)
    assert(bRead.toPathIfAllowed(onlyC, READ).isEmpty)
    assert(aWrite.toPathIfAllowed(onlyC, READ).isEmpty)
    assert(bWrite.toPathIfAllowed(onlyC, READ).isEmpty)
    assert(aRead.toPathIfAllowed(onlyC, WRITE).isEmpty)
    assert(bRead.toPathIfAllowed(onlyC, WRITE).isEmpty)
    assert(aWrite.toPathIfAllowed(onlyC, WRITE).isEmpty)
    assert(bWrite.toPathIfAllowed(onlyC, WRITE).isEmpty)

    assert(aRead.toPathIfAllowed(Set(Group("A")), READ).contains("A.read"))
    assert(bRead.toPathIfAllowed(Set(Group("B"), Group("A")), WRITE).contains("B.read"))
    assert(bWrite.toPathIfAllowed(Set(Group("B"), Group("C")), WRITE).contains("B.write"))
  }

  test("Operation.compare") {
    assert(Operation.READ <= Operation.READ)
    assert(!(Operation.WRITE <= Operation.READ))
    assert(Operation.READ < Operation.WRITE)
    assert(Operation.WRITE <= Operation.WRITE)
  }

}
