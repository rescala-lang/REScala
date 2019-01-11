import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._


object Settings {

  val version_211 = "2.11.12"
  val version_212 = "2.12.8"

  val strictScalacOptions = Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-target:jvm-1.8",
    "-Xlint",
    "-Xfuture",
    //"-Xlog-implicits" ,
    //"-Yno-predef" ,
    //"-Yno-imports" ,
    "-Xfatal-warnings",
    //"-Yinline-warnings" ,
    "-Yno-adapted-args",
    //"-Ywarn-dead-code" ,
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    //"-Ywarn-value-discard" ,
    )
  
  val compileWithStrictScalacOptions = Compile / compile / scalacOptions ++= strictScalacOptions
}

object Resolvers {
  val rmgk = Resolver.bintrayRepo("rmgk", "maven")
  val stg = Resolver.bintrayRepo("stg-tud", "maven")
  val all = Seq(rmgk, stg)
}

object Dependencies {
  val akkaHttp = libraryDependencies ++= Seq("akka-http-core", "akka-http").map(n => "com.typesafe.akka" %% n % "10.1.6")
  val akkaStream = libraryDependencies += ("com.typesafe.akka" %% "akka-stream" % "2.5.19")
  val betterFiles = libraryDependencies += ("com.github.pathikrit" %% "better-files" % "3.7.0")
  val circe = libraryDependencies ++= Seq("core", "generic", "generic-extras", "parser").map(n => "io.circe" %%% s"circe-$n" % "0.11.0")
  val decline = libraryDependencies += ("com.monovore" %% "decline" % "0.5.1")
  val fastparse = libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.1.0"
  val fontawesome = libraryDependencies += ("org.webjars" % "font-awesome" % "5.3.1")
  val jsoup = libraryDependencies += ("org.jsoup" % "jsoup" % "1.11.3")
  val lociDependency = Def.setting((n: String) => "de.tuda.stg" %%% s"scala-loci-$n" % "0.2.0")
  val lociCommunication = libraryDependencies ++= Seq("communication", "communicator-ws-akka").map(lociDependency.value)
  val lociCommunicationCirce =  libraryDependencies += lociDependency.value("serializer-circe")
  val lociCommunicationUpickle =  libraryDependencies += lociDependency.value("serializer-upickle")
  val pprint = libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.5.3"
  val purecss = libraryDependencies += ("org.webjars.npm" % "purecss" % "1.0.0")
  val rmgkLogging = libraryDependencies += ("de.rmgk" %%% "logging" % "0.2.1")
  val scalacheck = libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.0" % "test")
  val scalactic = libraryDependencies += ("org.scalactic" %% "scalactic" % "3.0.5")
  val scalajsdom = libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "0.9.6")
  val scalaswing = libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.3"
  val scalatags = libraryDependencies += ("com.lihaoyi" %%% "scalatags" % "0.6.7")
  val scalatest = libraryDependencies += ("org.scalatest" %% "scalatest" % "3.0.5" % "test")
  val scalaXml = libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
  val sourcecode = libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.4"
}
