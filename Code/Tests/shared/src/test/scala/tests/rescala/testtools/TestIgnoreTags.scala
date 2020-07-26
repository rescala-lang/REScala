package tests.rescala.testtools

import org.scalatest.{Ignore, Tag}

// workarounds for tests are fun.
case class IgnoreOnWindowsBecause(description: String)
    extends Tag(if (System.getProperty("os.name").contains("Windows")) classOf[Ignore].getName else "")

case class IgnoreOnGithubCiBecause(description: String)
    extends Tag(if (Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)) classOf[Ignore].getName else "")
