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

class LogRecorder {
  val logevents = new scala.collection.mutable.ListBuffer[LogEvent]
  def log(logevent: LogEvent) = logevents += logevent
}


case class LogNode(reactive: Reactive) {
  // CAREFUL: reference to node might impair garbage collection
  val identifier = System identityHashCode reactive
  val nodetype = reactive.getClass
  val level = reactive.level
}
class LogEvent
case class LogCreateNode(node: LogNode) extends LogEvent
case class LogAttachNode(node: LogNode, parent: LogNode) extends LogEvent
case class LogScheduleNode(node: LogNode) extends LogEvent
case class LogPulseNode(Node: LogNode) extends LogEvent
case class LogStartEvalNode(node: LogNode) extends LogEvent
case class LogEndEvalNode(node: LogNode) extends LogEvent
case class LogRound(stamp: Stamp) extends LogEvent

}