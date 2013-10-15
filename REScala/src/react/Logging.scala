package react

import scala.reflect.runtime.universe._
import java.io.PrintStream
import react.events.ImperativeEvent

class Inspect {
	scala.reflect.runtime.universe
}

package log {
  
class Logging {
  val loggers = new scala.collection.mutable.MutableList[Logger]
  def addLogger(logger: Logger) = loggers += logger
  def log(logevent: LogEvent) = loggers foreach (_ log logevent)
}
  
abstract class Logger(out: PrintStream) {
  def log(logevent: LogEvent)
}

trait LogRecorder extends Logger {
  val logevents = new scala.collection.mutable.ListBuffer[LogEvent]
  def log(logevent: LogEvent) = logevents += logevent
  def clear = logevents.clear
}


case class LogNode(reactive: Reactive, generictype: Type = null) {
  // CAREFUL: reference to node might impair garbage collection
  val identifier = System identityHashCode reactive
  val nodetype = reactive.getClass
  val level = reactive.level
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
    out.println("digraph G {")
    for(e <- logevents) { e match {
      case LogCreateNode(node) => 
        out.println(node.identifier + " [label=<<B>" + label(node) + ">]")
      case LogAttachNode(node, parent) =>
        out.println(parent.identifier + " -> " + node.identifier)
      case LogIFAttach(node, parent) =>
        out.println(parent.identifier + " -> " + node.identifier + " [style = dashed]")
      
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
  
  private def label(node: LogNode) = node.nodetype.getSimpleName
}

}