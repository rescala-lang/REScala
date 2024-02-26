package test.rdts

import rdts.base.Uid
import munit.Compare

implicit def idFromString(s: String): rdts.base.Uid = rdts.base.Uid.predefined(s)

given munit.Compare[rdts.base.Uid, String] = new Compare[Uid, String]:
  override def isEqual(obtained: Uid, expected: String): Boolean = Uid.unwrap(obtained) == expected

val isGithubCi: Boolean = (Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty))
