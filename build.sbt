import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Settings._
import Dependencies._

val commonSettings: sbt.Def.SettingsDefinition = Seq(
    scalaVersion := version_212,
    Compile / compile / scalacOptions ++= strictScalacOptions)

val Libraries = new {
  val shared = Def.settings(
    resolvers ++= Resolvers.all,
    rmgkLogging, scalatags, lociCommunication, circe
  )

  val main = shared ++ Def.settings(scalactic, jsoup, betterFiles, decline, akkaHttp, akkaStream, scalatest, scalacheck, betterFiles)

//  val mqttjs = jsDependencies += "org.webjars.npm" % "mqtt" % "2.18.2" / "2.18.2/mqtt.js" commonJSName "mqtt"
  val mqttjs = npmDependencies in Compile += "mqtt" -> "2.18.2"

  val js: Def.SettingsDefinition = shared ++ Seq(scalajsdom, purecss, fontawesome, mqttjs)
}

lazy val server = project.in(file("server"))
                  .settings(
                    name := "server",
                    commonSettings,
                    fork := true,
                    Libraries.main,
                    (Compile / compile) := ((Compile / compile) dependsOn (web / Compile / fastOptJS / webpack)).value,
                    Compile / compile := ((compile in Compile) dependsOn (web / Assets / SassKeys.sassify)).value,
                    (Compile / resources) += (web / Compile / fastOptJS / artifactPath).value,
                    (Compile / resources) ++= (web / Assets / SassKeys.sassify).value,
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


lazy val rescalatags = ProjectRef(base = file("../REScala"), id = "rescalatags")
lazy val rescalaJVM = ProjectRef(base = file("../REScala"), id = "rescalaJVM")
lazy val crdtsJVM = ProjectRef(base = file("../REScala"), id = "crdtsJVM")
lazy val crdtsJS = ProjectRef(base = file("../REScala"), id = "crdtsJS")



