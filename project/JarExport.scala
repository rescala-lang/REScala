/* This file is shared between multiple projects
 * and may contain unused dependencies */

import sbt.Keys.{crossTarget, fullClasspathAsJars, target}
import sbt.{Compile, File, IO, Setting, TaskKey}

// Extending sbt.AutoPlugin causes this plugin to be automatically added to all sbt projects that match the triggers.
// And because we don’t really specify any triggers, it is just added everywhere.
object JarExport extends sbt.AutoPlugin {
  override def trigger = allRequirements

  // a “task key” is something you can execute on the sbt commandline,
  // thus, this makes `stageJars` available like `compile` or `run`
  // though, it does not yet define behaviour
  val packageJars = TaskKey[File]("packageJars", "copies classpath jars to a file in the target dir")

  // second additional command, same as the above
  val writeClasspath = TaskKey[File]("writeClasspath", "writes the classpath to a file in the target dir")

  // This defines settings the plugin makes.
  // It is essentially the same as if this was in a `.settings()` block in the build.sbt
  override lazy val projectSettings: Seq[Setting[?]] = Seq(
    // copy all jars required in the class path to a `jars` folder in the target directory
    packageJars := {
      val cp         = (Compile / fullClasspathAsJars).value
      val targetpath = crossTarget.value.toPath.resolve("jars")
      IO.delete(targetpath.toFile)
      IO.createDirectory(targetpath.toFile)
      cp.foreach { at =>
        IO.copyFile(at.data, targetpath.resolve(at.data.getName).toFile)
      }
      // the return value is what `show stageJars` will display
      targetpath.toFile
    },

    // write the classpath into a file that can be passed to java as a commandline argument file
    writeClasspath := {
      val cp = (Compile / fullClasspathAsJars).value
      val cpstring = cp.map { at =>
        val pathstring = at.data.toString.replace("\\", "/")
        s"""-cp "${pathstring}"\n"""
      }.mkString("")
      val targetpath = crossTarget.value.toPath.resolve("classpath.txt")
      IO.write(targetpath.toFile, cpstring)
      targetpath.toFile
    }
  )

}
