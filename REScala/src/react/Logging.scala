package react

import scala.reflect.runtime.universe._
import java.io.PrintStream
import react.events.ImperativeEvent
import react.events.Event

class Inspect {
	scala.reflect.runtime.universe
}

package log {
  
object Logging {
  val DefaultPathPrefix = "./logs/"
}
  
class Logging {
  val loggers = new scala.collection.mutable.MutableList[Logger]
  def addLogger(logger: Logger) = loggers += logger
  def log(logevent: LogEvent) = loggers foreach (_ log logevent)
  
  def enableDefaultLogging {   
    val maybeName = for(main <- 
      Thread.getAllStackTraces.keySet().toArray().find(_.asInstanceOf[Thread].getName == "main")) 
      yield main.asInstanceOf[Thread].getStackTrace.last.getFileName
    val name = maybeName.getOrElse("LOG").takeWhile(_ != '.')
    
    val dotfile = Logging.DefaultPathPrefix + name + ".dot"
    val dotGraphLogger = new react.log.DotGraphLogger(
      new java.io.PrintStream(
      new java.io.FileOutputStream(dotfile, false)))
    
    val reactplayerfile = Logging.DefaultPathPrefix + name + ".txt"
    val reactplayerLogger = new react.log.ReactPlayerLog(
      new java.io.PrintStream(
      new java.io.FileOutputStream(reactplayerfile, false)))
   
    //addLogger(new SimpleLogger(System.out))
    addLogger(reactplayerLogger)
    addLogger(dotGraphLogger)
    
    scala.sys.addShutdownHook{ dotGraphLogger.snapshotOnce }
  }
}

abstract class Logger(out: PrintStream) {
  def log(logevent: LogEvent)
}

trait LogRecorder extends Logger {
  val logevents = new scala.collection.mutable.ListBuffer[LogEvent]
  def log(logevent: LogEvent) = logevents += logevent
  def clear = logevents.clear
}


case class LogNode(reactive: Reactive) {
  // CAREFUL: reference to node might impair garbage collection
  // Possible solution: somehow discard reference to 'reactive', store only what is needed
  val identifier = System identityHashCode reactive
  val level = reactive.level
  val nodetype = reactive.getClass
  
  lazy val typename = getParametricType(reactive) // "lazy" is key here!  
  private def getParametricType(r: Reactive, primitive: Boolean = false): String = {
    // ugly, but still better than fiddling with TypeTags or other reflection
    val typename = r.getClass.getSimpleName
    if(r.isInstanceOf[Var[_]]){
      val thistype = if(primitive) "Var" else typename
      val value = r.asInstanceOf[Var[_]].getValue
      return thistype + "["+ getInner(value) + "]"
    }
    else if(r.isInstanceOf[Signal[_]]){
      val thistype = if(primitive) "Signal" else typename
      val value = r.asInstanceOf[Signal[_]].getValue
      return thistype + "["+ getInner(value) + "]"
    }
    else if(r.isInstanceOf[Event[_]]) {
      return typename
    }

    def getInner(value: Any) = 
      if(value == null) "?" else 
      if(value.isInstanceOf[Reactive]) getParametricType(value.asInstanceOf[Reactive]) 
      else value.getClass.getSimpleName
      
    return reactive.getClass.getSimpleName // should not be reached
  }
}
class LogEvent
case class LogMessage(string: String) extends LogEvent
case class LogCreateNode(node: LogNode) extends LogEvent
case class LogAttachNode(node: LogNode, parent: LogNode) extends LogEvent
case class LogScheduleNode(node: LogNode) extends LogEvent
case class LogPulseNode(Node: LogNode) extends LogEvent
case class LogStartEvalNode(node: LogNode) extends LogEvent
case class LogEndEvalNode(node: LogNode) extends LogEvent
case class LogRound(stamp: Stamp) extends LogEvent
case class LogIFAttach(node: LogNode, parent: LogNode) extends LogEvent // "virtual" association through IF


class SimpleLogger(out: PrintStream) extends Logger(out) {
  def log(logevent: LogEvent) = out.println(logevent)
}

class ReactPlayerLog(out: PrintStream) extends Logger(out) {
  var timestamp = 0
  def log(logevent: LogEvent) = logevent match {
    case LogRound(round) => timestamp = round.roundNum * 1000 + round.sequenceNum
    case LogCreateNode(node) =>
      out.println("NodeCreate : " + timestamp)
      out.println("> Node = " + node.identifier)
      out.println("> Type = " + getTypeName(node))
    case LogAttachNode(node, parent) =>
      out.println("NodeAttach : " + timestamp)
      out.println("> Node = " + node.identifier)
      out.println("> Parent = " + parent.identifier)      
    case LogPulseNode(node) =>
      out.println("NodePulse : " + timestamp)
      out.println("> Node = " + node.identifier)
      out.println("> Transaction = " + node.level)
    case LogStartEvalNode(node) =>
      out.println("NodeEvaluateBegin : " + timestamp)
      out.println("> Node = " + node.identifier)
      out.println("> Transaction = " + node.level)
      out.println("> Type = " + getTypeName(node))
    case LogEndEvalNode(node) =>
      out.println("NodeEvaluateEnd : " + timestamp)
      out.println("> Node = " + node.identifier)
      out.println("> Transaction = " + node.level)
      out.println("> Type = " + getTypeName(node))      
    case _ =>
  }
 
  val VarNodeClass = classOf[VarSynt[_]]
  val FunctionNodeClass = classOf[SignalSynt[_]]
  def getTypeName(node: LogNode): String = node.nodetype match {
    case VarNodeClass => "VarNode"
    case FunctionNodeClass => "FunctionNode"
    case x => x.getSimpleName
  }
}

/** Outputs a graph in dot format, at the moment the "snapshot" function is called */
class DotGraphLogger(out: PrintStream) extends Logger(out) with LogRecorder {
  
  def snapshot {
    if(logevents.isEmpty) return
    
    val existingLinks = new collection.mutable.HashSet[(Int, Int)]
    def doIfLinkNotPresent(fromTo: (Int, Int))(body: => Unit){
      if(!existingLinks.contains(fromTo)){
        existingLinks.add(fromTo)
        existingLinks.add(fromTo.swap)
        body
      }
    }
    
    out.println("digraph G {")
    for(e <- logevents) { e match {
      case LogCreateNode(node) => 
        out.println(node.identifier + " [label=<" + label(node) + ">]")
      case LogAttachNode(node, parent) => 
        doIfLinkNotPresent(node.identifier, parent.identifier) {
        	out.println(parent.identifier + " -> " + node.identifier)
        }
      case LogIFAttach(node, parent) =>
        doIfLinkNotPresent(node.identifier, parent.identifier) {
        	out.println(parent.identifier + " -> " + node.identifier + " [style = dashed]")
        }
      
      case LogMessage(s) => out.println("// " + s)
      case other => 
    }}
    out.println("}")
    //clear
  }
  
  
  /** Performs a snapshot the first time this is called, subsequent calls are ignored */
  def snapshotOnce {
    if(snapshotDone) return
    snapshotDone = true
    snapshot    
  }
  
  private var snapshotDone = false
  
  private def label(node: LogNode) = node.typename
}

}