package tests.rescala.testtools

// workarounds for tests are fun.
// note, this does not seem to work in JS because the environment variable can not be read
case class IgnoreOnWindowsBecause(description: String)
//extends Tag(if (System.getProperty("os.name").contains("Windows")) "org.scalatest.Ignore" else "")

case class IgnoreOnGithubCiBecause(description: String)
//extends Tag(if (Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)) "org.scalatest.Ignore" else "")

case class IgnoreOnGithubWindowsCiBecause(description: String)
//    extends Tag(
//      if (
//        System.getProperty("os.name").contains("Windows") &&
//        Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)
//      ) "org.scalatest.Ignore"
//      else ""
//    )
