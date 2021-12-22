package tests.rescala.testtools

import org.scalatest.{Ignore, Tag}

// workarounds for tests are fun.
case class IgnoreOnWindowsBecause(description: String)
    extends Tag(if (System.getProperty("os.name").contains("Windows")) "Ignore" else "")

case class IgnoreOnGithubCiBecause(description: String)
    extends Tag(if (Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)) "Ignore" else "")

case class IgnoreOnGithubWindowsCiBecause(description: String)
    extends Tag(
      if (
        System.getProperty("os.name").contains("Windows") &&
        Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)
      ) "Ignore"
      else ""
    )
