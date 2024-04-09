ThisBuild / scalaVersion := "3.4.1"
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-release:21",
  "-deprecation",
  "-Ysafe-init",
  "-explain"
)
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "CRDT ACLs",
    libraryDependencies ++= Seq(
      "org.scalameta"    %% "munit"                        % "1.0.0-M11" % Test,
      "org.bouncycastle"  % "bcprov-jdk18on"               % "1.78",
      "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.78",
      "io.github.hakky54" % "sslcontext-kickstart"         % "8.3.4",
      "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "8.3.4",
      "org.slf4j"         % "slf4j-jdk14"                  % "2.0.12"
    )
  )

lazy val encrdt = project
  .in(file("RDTs"))
  .settings(
    name := "encrdt",
    libraryDependencies += jsoniterCoreDependency,
    libraryDependencies ++= encrdtDependencies ++ scalatestDependency
  )

lazy val encrdtDependencies = Seq(
  // Encryption / Decryption using Googles Tink Crypto Library
  "com.google.crypto.tink"      % "tink"          % "1.13.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

// jsoniter-scala
lazy val jsoniterCoreDependency = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.4"
lazy val jsoniterMacroDependency =
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % Provided

// scalatest
lazy val scalatestDependency = Seq(
  "org.scalactic" %% "scalactic" % "3.2.18",
  "org.scalatest" %% "scalatest" % "3.2.18" % "test"
)
