lazy val chat =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full).in(file("."))
    .settings(
      scalaVersion := "3.4.1",
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies ++= List(
        "de.tu-darmstadt.stg" %%% "rescala" % "0.35.1",
        "de.tu-darmstadt.stg" %%% "kofre"   % "0.35.1",

        "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"                  % "2.28.4",
        "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros"                % "2.28.4",
        "com.softwaremill.sttp.client4"         %%% "core"                                 % "4.0.0-M11",
        "io.bullet"                             %%% "borer-core"                           % "1.14.0",
        "io.bullet"                             %%% "borer-derivation"                     % "1.14.0"
      )
    )
    .jsSettings(
      scalaVersion := "3.4.1",
      libraryDependencies ++= List(
        "org.scala-js"                      %%% "scalajs-dom"                          % "2.8.0",
        "com.lihaoyi"                       %%% "scalatags"                            % "0.12.0",
      )
    )
    .jvmSettings(
      scalaVersion := "3.4.1"
    )
