import Settings.scala3defaults

import java.net.URI
import scala.scalanative.build.{LTO, Mode}

lazy val bismuth = project.in(file(".")).settings(scala3defaults).aggregate(
  aead.js,
  aead.jvm,
  channels.js,
  channels.jvm,
  dtn.js,
  dtn.jvm,
  deltalens,
  exampleLenses,
  examplesMiscJVM,
  loCal,
  lofiAcl,
  lofiAclExample,
  lore.js,
  lore.jvm,
  loreCompilerPlugin,
  microbenchmarks,
  rdts.js,
  rdts.jvm,
  rdts.native,
  reactives.js,
  reactives.jvm,
  reactives.native,
  replication.js,
  replication.jvm,
  replication.native,
  replicationExamples.js,
  replicationExamples.jvm,
  todolist
)

// aggregate projects allow compiling all variants (js, jvm, native) at the same time

lazy val rdtsAggregate =
  project.in(file("target/PhonyBuilds/rdts")).settings(scala3defaults, publish / skip := true)
    .aggregate(rdts.js, rdts.jvm, rdts.native)

lazy val reactivesAggregate =
  project.in(file("target/PhonyBuilds/reactives")).settings(scala3defaults, publish / skip := true)
    .aggregate(reactives.js, reactives.jvm, reactives.native)

// projects in alphabetical order

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin))
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmSettings(
    LocalSettings.tink
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers" -> "0.7.13",
    )
  )

lazy val channels = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels"))
  .dependsOn(rdts)
  .settings(
    Settings.scala3defaults,
    // jetty 12 requires java 17
    Settings.javaOutputVersion(17, Test / compile),
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.slips.delay,
    Dependencies.munit,
  )
  .jsSettings(
    Settings.jsEnvDom,
    Dependencies.scalajsDom,
    Dependencies.scalatags,
  )
  .jvmSettings(
    Test / fork := true,
    libraryDependencies ++= LocalSettings.jetty.map(_ % Provided),
    LocalSettings.slf4jSimpleTest,
  )

lazy val deltalens = project.in(file("Modules/Deltalens"))
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.munit,
    libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
      "org.scalatest" %%% s"scalatest-$m" % "3.2.19" % Test
    ),
  )

lazy val dtn = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/DTN"))
  .dependsOn(reactives, rdts, replication)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    libraryDependencies ++= List(
      "com.softwaremill.sttp.client4" %%% "core"             % "4.0.0-M17",
      "io.bullet"                     %%% "borer-core"       % "1.14.1",
      "io.bullet"                     %%% "borer-derivation" % "1.14.1"
    )
  )

lazy val exampleLenses = project.in(file("Modules/Examples/ReactiveLenses"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(reactives.js)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.scalatags,
    LocalSettings.deployTask,
  )

lazy val examplesMiscJVM = project.in(file("Modules/Examples/Misc JVM"))
  .enablePlugins(JmhPlugin)
  .dependsOn(reactives.jvm, replication.jvm)
  .settings(
    scala3defaults,
    fork := true,
    Dependencies.jsoniterScala,
    Dependencies.munitCheck,
    LocalSettings.tink,
    libraryDependencies += LocalSettings.scalafx,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"   % "2.3.0",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    ),
    libraryDependencies += "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
    Settings.implicitConversions(), // reswing uses this in a million places for no reason
  )

lazy val loCal = project.in(file("Modules/Examples/Lore Calendar"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rdts.js, reactives.js, channels.js, lore.js, replication.js)
  .settings(
    scala3defaults,
    Settings.resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    LocalSettings.deployTask
  )

lazy val lofiAcl = project.in(file("Modules/Local-first Access Control"))
  .dependsOn(deltalens, rdts.jvm % "compile->compile;test->test")
  .settings(
    scala3defaults,
    // SunEC crypto provider does not support Ed25519 in jdk 11
    Settings.javaOutputVersion(17),
    Settings.safeInit(Compile / compile, Test / compile),
    Dependencies.munit,
    Dependencies.munitCheck,
    Dependencies.jsoniterScala,
    LocalSettings.tink,
    libraryDependencies ++=
      List(
        // Note, the below means JDK 1.4
        "org.slf4j" % "slf4j-jdk14" % "2.0.16",
        // Note, the below means JDK 1.8, aka Java 8
        "org.bouncycastle"  % "bcprov-jdk18on"               % "1.78.1",
        "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.78.1",
        "io.github.hakky54" % "sslcontext-kickstart"         % "8.3.6",
        "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "8.3.6",
      ),
    Test / fork := true,
  )

lazy val lofiAclExample = project.in(file("Modules/Local-first Access Control/Example"))
  .dependsOn(lofiAcl)
  .settings(
    scala3defaults,
    libraryDependencies += LocalSettings.scalafx,
    Dependencies.jsoniterScala,
    Dependencies.munit,
    publish / skip := true,
  )

lazy val lore = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Dependencies.jsoniterScala,
    libraryDependencies += "com.monovore"  %%% "decline"      % "2.4.1",
    libraryDependencies += "org.typelevel" %%% "cats-parse"   % "1.0.0",
    libraryDependencies += "com.lihaoyi"   %%% "fansi"        % "0.5.0",
    libraryDependencies += "dev.optics"    %%% "monocle-core" % "3.3.0",
    Dependencies.munit,
    Compile / mainClass := Some("lore.Compiler")
  )

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm)
  .settings(
    scala3defaults,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    Dependencies.munit
  )

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .dependsOn(lore.jvm)
  .settings(
    scala3defaults,
    Dependencies.munit,
    scalacOptions += {
      val pluginClasspath = (loreCompilerPlugin / Compile / fullClasspathAsJars).value
        .map(at => at.data).mkString(java.io.File.pathSeparator)
      s"-Xplugin:${pluginClasspath}"
    }
  )

lazy val microbenchmarks = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(reactives.jvm, rdts.jvm, replication.jvm, lofiAcl)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    Settings.jolSettings,
    LocalSettings.tink,
    libraryDependencies += "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
  )

lazy val rdts = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(8),
    Settings.safeInit(Compile / compile),
    Settings.explicitNulls(Compile / compile),
    LocalSettings.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jsSettings(
    Settings.sourcemapFromEnv()
  )

lazy val reactives = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(9), // for java.util.Flow
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    LocalSettings.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
  )
  .jvmSettings(
    libraryDependencies += LocalSettings.scalafx % Provided,
  )
  .jsSettings(
    Dependencies.scalajsDom,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % Test,
    Settings.jsEnvDom,
    Settings.sourcemapFromEnv(),
  )

lazy val replication = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Replication"))
  .dependsOn(reactives, rdts, channels, rdts % "compile->compile;test->test")
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(11), // java webserver
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  )

lazy val replicationExamples = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
  .in(file("Modules/Examples/Replication"))
  .dependsOn(replication)
  .settings(
    scala3defaults,
    run / fork         := true,
    run / connectInput := true,
    Settings.resolverJitpack,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.scalacheck,
    Dependencies.slips.options,
    Dependencies.slips.delay,
    Dependencies.jsoniterScala,
    Settings.strictEquality(Compile / compile, Test / test),
  )
  .jvmSettings(
    Dependencies.slips.script,
    Dependencies.sqliteJdbc,
    LocalSettings.slf4jSimpleTest,
    libraryDependencies ++= LocalSettings.jetty,
  )
  .jsSettings(
    Dependencies.scalatags,
    LocalSettings.deployTask,
  )

lazy val todolist = project.in(file("Modules/Examples/TodoMVC"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(replication.js, dtn.js)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Settings.resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    LocalSettings.deployTask,
    Dependencies.pprint,
  )

lazy val webview = project.in(file("Modules/Webview"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(replication.native)
  .settings(
    Settings.scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    FetchResources.fetchedResources += FetchResources.ResourceDescription(
      (Compile / unmanagedResourceDirectories).value.head.toPath.resolve("scala-native/webview.h"),
      URI.create(
        "https://raw.githubusercontent.com/webview/webview/93be13a101e548c13d47ae36a6ea00300b2ecfc0/webview.h"
      ),
      "593cbc6714e5ea1239006991fff0cad55eee02b7"
    ),
    nativeLink := (Compile / nativeLink).dependsOn(FetchResources.fetchResources).value,
    nativeConfig ~= { c =>
      val d = c.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withIncrementalCompilation(true)
      // The below disables LTO for macos as that seems to cause problems.
      // Windows not implemented, macos has known issues.
      LocalSettings.osSpecificWebviewConfig(d)
    }
  )
