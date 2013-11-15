package react.log

import react.Reactive
import react.Stamp

abstract class Logging {
  def log(logevent: LogEvent)
  def node(reactive: Reactive): LogNode
}

abstract class Logger {
  def log(logevent: LogEvent)
}

abstract class LogNode(reactive: Reactive){
  val identifier: Int
  val level: Int
  val nodetype: Class[_]
  val simpletype: Class[_]
  val typename: String
  val meta: NodeMetaInfo
}

object NodeMetaInfo {
  val NoVarName = "?"
}
case class NodeMetaInfo(val varname: String)

class LogEvent { val time = System.currentTimeMillis }
case class LogMessage(string: String) extends LogEvent
case class LogCreateNode(node: LogNode) extends LogEvent
case class LogAttachNode(node: LogNode, parent: LogNode) extends LogEvent
case class LogScheduleNode(node: LogNode) extends LogEvent
case class LogPulseNode(Node: LogNode) extends LogEvent
case class LogStartEvalNode(node: LogNode) extends LogEvent
case class LogEndEvalNode(node: LogNode) extends LogEvent
case class LogRound(stamp: Stamp) extends LogEvent
case class LogIFAttach(node: LogNode, parent: LogNode) extends LogEvent // "virtual" association through IF


/** A Logging stub, that performs no logging */
object NoLogging extends Logging {
  def log(logevent: LogEvent) {}
  def node(reactive: Reactive): LogNode = null
}