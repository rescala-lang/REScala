package test.kofre

import munit.Compare
import kofre.base.Uid

implicit def idFromString(s: String): kofre.base.Uid = kofre.base.Uid.predefined(s)

given munit.Compare[kofre.base.Uid, String] = new Compare[Uid, String]:
  override def isEqual(obtained: Uid, expected: String): Boolean = Id.unwrap(obtained) == expected
