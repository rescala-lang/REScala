import java.nio.file.Files

import Dependencies._
import Settings._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val commonSettings: sbt.Def.SettingsDefinition = Def.settings(
  scalaVersion_212,
  strictCompile)

val Libraries = new {
  val shared = Def.settings(
    rmgkLogging, scalatags, loci.communication, loci.wsAkka, circe
  )

  val main = shared ++ Def.settings(jsoup,
                                    betterFiles,
                                    decline,
                                    akkaHttp)

  val npmDeps = npmDependencies in Compile ++= Seq("mqtt" -> "2.18.2")


  val js: Def.SettingsDefinition = shared ++ Seq(scalajsdom, npmDeps, normalizecss)
}

val vbundle = TaskKey[File]("vbundle", "bundles all the viscel resources")
val vbundleDef = vbundle := {
  val jsfiles = (web / Compile / fastOptJS / webpack).value
  val styles = (web / Assets / SassKeys.sassify).value
  val bundleTarget = (Universal / target).value.toPath.resolve("stage/resources")
  Files.createDirectories(bundleTarget)
  Files.createDirectories(bundleTarget.resolve("static"))

  def gzipToTarget(f: File): Unit = IO.gzip(f, bundleTarget.resolve(f.name + ".gz").toFile)

  jsfiles.foreach { af =>
    val jsfile = af.data
    gzipToTarget(jsfile)
    val map = jsfile.toPath.getParent.resolve(jsfile.name + ".map").toFile
    if (map.exists()) gzipToTarget(jsfile.toPath.getParent.resolve(jsfile.name + ".map").toFile)
  }

  val staticResources = (Compile / resources).value
  val resdir = (Compile / resourceDirectory).value
  staticResources.filter(_.isFile).foreach { f =>
    IO.copyFile(f,
                bundleTarget.resolve(resdir.relativize(f).get.toPath).toFile)
  }
  styles.foreach(gzipToTarget)
  bundleTarget.toFile
}

lazy val server = project.in(file("server"))
                  .settings(
                    name := "server",
                    commonSettings,
                    fork := true,
                    Libraries.main,
                    vbundleDef
                  )
                  .enablePlugins(JavaServerAppPackaging)
                  .dependsOn(sharedJVM)
                  .dependsOn(rescalaJVM, crdtsJVM)

lazy val web = project.in(file("web"))
               .enablePlugins(ScalaJSPlugin)
               .settings(
                 name := "web",
                 commonSettings,
                 Libraries.js,
                 scalaJSUseMainModuleInitializer := true,
                 webpackBundlingMode := BundlingMode.LibraryOnly(),
                 )
               .dependsOn(sharedJS)
               .enablePlugins(SbtSassify)
               .enablePlugins(ScalaJSBundlerPlugin)
               .dependsOn(rescalatags, crdtsJS)

lazy val shared = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).in(file("shared"))
                  .settings(
                    name := "shared",
                    commonSettings,
                    Libraries.shared,
                    )
                  .jsConfigure(_.dependsOn(crdtsJS))
                  .jvmConfigure(_.dependsOn(crdtsJVM))
lazy val sharedJVM = shared.jvm
lazy val sharedJS = shared.js


lazy val rescalaRepo = uri("git://github.com/rescala-lang/REScala.git#" +
                           "3a8224d382151f51aae8d9da4ca161f6a617c525")
lazy val rescalatags = ProjectRef(rescalaRepo, "rescalatags")
lazy val rescalaJVM = ProjectRef(rescalaRepo, "rescalaJVM")
lazy val crdtsJVM = ProjectRef(rescalaRepo, "crdtsJVM")
lazy val crdtsJS = ProjectRef(rescalaRepo, "crdtsJS")



