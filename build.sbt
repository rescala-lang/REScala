val scala3Version = "3.4.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "CRDT ACLs",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-feature",
      "-release:21",
      "-deprecation",
      "-Ysafe-init"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta"    %% "munit"                        % "0.7.29" % Test,
      "org.bouncycastle"  % "bcprov-jdk18on"               % "1.77",
      "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.77",
      "io.github.hakky54" % "sslcontext-kickstart"         % "8.3.4",
      "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "8.3.4",
      "org.slf4j"         % "slf4j-jdk14"                  % "2.0.12"
    )
  )
