ThisBuild / scalaVersion := "3.2.1"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github.ckuessner"

lazy val root = (crossProject(JSPlatform, JVMPlatform) in file("."))
  .settings(
    name := "aead",
    libraryDependencies ++= Seq(
      "org.scalatest"     %%% "scalatest"       % "3.2.15"   % "test",
      "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.15.0" % "test"
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.google.crypto.tink" % "tink" % "1.7.0"
    )
  )
  .jsConfigure(_.enablePlugins(ScalablyTypedConverterPlugin))
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers"        -> "0.7.10",
      "@types/libsodium-wrappers" -> "0.7.10"
    )
  )
