package react.log

import react._
import scala.reflect.runtime.universe._
import java.io.PrintStream
import java.io.File

object NodeMetaInfo {
  val NoVarName = "?"
}
case class NodeMetaInfo(val varname: String)

object SrcReader {
  
  /** This method should be called EXACTLY when a reactive is created.
   *  By inspecting the stack trace and source files, it determines the variable name of the current object
   */
  def getMetaInfo(reactive: Reactive) = metaInfo.getOrElseUpdate(reactive, createMetaInfo)  
  val metaInfo = new scala.collection.mutable.HashMap[Reactive, NodeMetaInfo]  
  
  private def createMetaInfo: NodeMetaInfo = {
    val varname =
    Thread.currentThread().getStackTrace().filterNot {s => val c = s.getClassName
	    c.startsWith("java") || c.startsWith("scala") || c.startsWith("react")}.headOption match {
	      case Some(trace) => 
	        getVarName(trace.getFileName(), trace.getClassName(), trace.getLineNumber())
	      case None => NodeMetaInfo.NoVarName
	    }
	NodeMetaInfo(varname)
  }

  var sourceFolder = MyLogging.DefaultSourceFolder
  def setSourceFolder(s: String) { sourceFolder = s }
  
  lazy val sourceFiles = findSourceFiles(new File(sourceFolder))
  private def findSourceFiles(path: File): List[File] = {
    val files = path.listFiles.toList
    val recursive = (files.filter { _.isDirectory }.flatMap { findSourceFiles(_) })
    val here = files.filterNot { _.isDirectory }.filter(_.getName().endsWith(".scala"))
    here ::: recursive
  }
  
  private def getVarName(filename: String, pathhint: String, linenum: Int): String = {
    val file = getFile(filename, pathhint)
    if(file.isEmpty) return NodeMetaInfo.NoVarName
    val varnames = getFileVarnames(file.get)
    varnames.getOrElse(linenum, NodeMetaInfo.NoVarName)
  }
  
  private def getFile(filename: String, pathhint: String): Option[File] = {
    val candidates = sourceFiles.filter(_.getName().endsWith(filename))
    candidates.size match {
      case 0 => None
      case 1 => Some(candidates.head)
      case _ => 
	      val refset = pathhint.split('.').toSet
	      val best = candidates.maxBy(
	          _.getPath().stripSuffix(filename).split("[/\\\\]").toSet.intersect(refset).size)
	      Some(best)
    }
  }
  
  val fileVarnames = new scala.collection.mutable.HashMap[String, Map[Int, String]]
  private def getFileVarnames(file: File): Map[Int, String] = {
    fileVarnames.getOrElseUpdate(file.getPath(), parseFile(file))
  }
  
  val varRegex = new scala.util.matching.Regex("^\\s*(?:val|var|def)\\s+(\\w+)(?::.*)?\\s+=", "varname")
  private def parseFile(file: File): Map[Int, String] = {
    val lines = scala.io.Source.fromFile(file).getLines
    val vardefs = for {(line: String, i: Int) <- lines.zipWithIndex
          vardef <- varRegex.findFirstMatchIn(line)}
        yield (i + 1, vardef.group("varname"))
    vardefs.toMap
  }

}