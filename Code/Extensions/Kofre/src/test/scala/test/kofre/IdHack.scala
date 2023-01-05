package test.kofre

import munit.Compare
import kofre.base.Id

implicit def idFromString(s: String): kofre.base.Id = kofre.base.Id.predefined(s)

given munit.Compare[kofre.base.Id, String] = new Compare[Id, String]:
  override def isEqual(obtained: Id, expected: String): Boolean = Id.unwrap(obtained) == expected
