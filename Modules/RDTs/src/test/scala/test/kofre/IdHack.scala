package test.kofre

import kofre.base.Uid
import munit.Compare

implicit def idFromString(s: String): kofre.base.Uid = kofre.base.Uid.predefined(s)

given munit.Compare[kofre.base.Uid, String] = new Compare[Uid, String]:
  override def isEqual(obtained: Uid, expected: String): Boolean = Uid.unwrap(obtained) == expected


val isGithubCi: Boolean = (Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty))
