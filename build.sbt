lazy val chat =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full).in(file(".")) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(
      scalaVersion := "3.3.3",
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies ++= List(
        "de.tu-darmstadt.stg" %%% "rescala" % "0.35.1",
        "de.tu-darmstadt.stg" %%% "kofre"   % "0.35.1",

        // Below imports are required for the restoration and distribution.
        "com.github.scala-loci.scala-loci"      %%% "scala-loci-communicator-webrtc"       % "b9809c9c2d",
        "com.github.scala-loci.scala-loci"      %%% "scala-loci-serializer-jsoniter-scala" % "b9809c9c2d",
        "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"                  % "2.28.4",
        "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros"                % "2.28.4",
        "com.softwaremill.sttp.client4"         %%% "core"                                 % "4.0.0-M11",
        "io.bullet"                             %%% "borer-core"                           % "1.14.0",
        "io.bullet"                             %%% "borer-derivation"                     % "1.14.0"
      )
    )
    .jsSettings(
      scalaVersion := "3.3.3",
      libraryDependencies ++= List(
        "org.scala-js" %%% "scalajs-dom" % "2.8.0",
        "com.lihaoyi"  %%% "scalatags"   % "0.12.0",
      )
    )
    .jvmSettings(
      scalaVersion := "3.3.3",
      libraryDependencies ++= List(
        "com.softwaremill.sttp.client4" %% "okhttp-backend" % "4.0.0-M11",
      )
    )
