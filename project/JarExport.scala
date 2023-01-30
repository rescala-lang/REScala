/* This file is shared between multiple projects
 * and may contain unused dependencies */

import sbt.Keys.{fullClasspathAsJars, target}
import sbt.{Compile, IO, Setting, TaskKey}
import sbt.*

object JarExport extends sbt.AutoPlugin {
  override def trigger = allRequirements

  val writeClasspath = TaskKey[File]("writeClasspath", "writes the classpath to a file in the target dir")
  val stageJars      = TaskKey[File]("stageJars", "copies classpath jars to a file in the target dir")

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    // util to generate classpath file to be consumed by native image
    writeClasspath := {
      val cp         = (Compile / fullClasspathAsJars).value
      val cpstring   = cp.map(at => s"""-cp "${at.data.toString.replace("\\", "/")}"\n""").mkString("")
      val targetpath = target.value.toPath.resolve("classpath.txt")
      IO.write(targetpath.toFile, cpstring)
      targetpath.toFile
    },
    // util to copy classpath into a local folder
    stageJars := {
      val cp         = (Compile / fullClasspathAsJars).value
      val targetpath = target.value.toPath.resolve("jars")
      IO.delete(targetpath.toFile)
      IO.createDirectory(targetpath.toFile)
      cp.foreach { at =>
        IO.copyFile(at.data, targetpath.resolve(at.data.getName).toFile)
      }
      targetpath.toFile
    }
  )
}
