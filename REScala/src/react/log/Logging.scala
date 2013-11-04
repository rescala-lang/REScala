package react.log

import react._
import scala.reflect.runtime.universe._
import java.io.PrintStream
import react.events.Event
import java.io.File
import react.events.EventHandler
import react.events.ImperativeEvent

  
object Logging {
  val DefaultPathPrefix = "./logs/"
  val DefaultSourceFolder = "./src/"
}

object ReactiveTypes {
  
  val HandlerClass = classOf[EventHandler[_]]
  val ImperativeClass = classOf[ImperativeEvent[_]]
  val EventClass = classOf[Event[_]]
  val VarClass = classOf[Var[_]]
  val SignalClass = classOf[Signal[_]]
  // Some leaf classes:
  val VarLeafClass = classOf[VarSynt[_]]
  val SignalLeafClass = classOf[SignalSynt[_]]
  
  /** Simplifies the type of a reactive to one of the given basic types */
  def getSimpleType(reactive: Reactive): Class[_] = {
	List(ImperativeClass, VarClass, SignalClass, HandlerClass, EventClass).
		find(_.isInstance(reactive)) getOrElse (throw new RuntimeException("Class not found"))
  }
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
    
    val statsfile = Logging.DefaultPathPrefix + name + "_stats.txt"
    val statslogger = new StatisticsLogger(
      new java.io.PrintStream(
      new java.io.FileOutputStream(statsfile, false)));
   
    //addLogger(new SimpleLogger(System.out))
    addLogger(reactplayerLogger)
    addLogger(dotGraphLogger)
    addLogger(statslogger)
  }
  
}

abstract class Logger(out: PrintStream) {
  def log(logevent: LogEvent)
}

trait LogRecorder extends Logger {
  val logevents = new scala.collection.mutable.ListBuffer[LogEvent]
  def log(logevent: LogEvent) = logevents += logevent
  def clear = logevents.clear
  
  def snapshot
  
  
  /** Performs a snapshot the first time this is called, subsequent calls are ignored */
  def snapshotOnce {
    if(snapshotDone) return
    snapshotDone = true
    snapshot    
  }
  private var snapshotDone = false
  
  // If the snapshot was never called manually, snapshot on program exit
  scala.sys.addShutdownHook{ snapshotOnce }
}


case class LogNode(reactive: Reactive) {
  // CAREFUL: reference to node might impair garbage collection
  // Possible solution: somehow discard reference to 'reactive', store only what is needed

  val identifier = System identityHashCode reactive
  val level = reactive.level
  val nodetype = reactive.getClass
  val simpletype = ReactiveTypes.getSimpleType(reactive)
  val meta = SrcReader.getMetaInfo(reactive) // important: This MUST be called when the reactive is first created
  lazy val typename = getParametricType(reactive) // "lazy" is key here!
  
  private def getParametricType(r: Reactive, primitive: Boolean = false): String = {
    // ugly, but still better than fiddling with TypeTags or other reflection
    val typename = r.getClass.getSimpleName
    
    if(ReactiveTypes.VarClass.isInstance(r)){
      val thistype = if(primitive) "Var" else typename
      val value = r.asInstanceOf[Var[_]].getValue
      return thistype + "["+ getInner(value) + "]"
    }
    else if(ReactiveTypes.SignalClass.isInstance(r)){
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
  def getTypeName(node: LogNode): String = node.simpletype match {
    case ReactiveTypes.VarClass => "VarNode"
    case ReactiveTypes.SignalClass => "FunctionNode"
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
    out.println("node [shape=box]")
    for(e <- logevents) { e match {
      case LogCreateNode(node) => 
        out.println(node.identifier + " [label=<" + label(node) + "> " + style(node) + "]")
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
  
  private def style(node: LogNode): String = {    
    "style=filled fillcolor=" + (node.simpletype match {
      case ReactiveTypes.ImperativeClass => "brown1"
      case ReactiveTypes.VarClass => "coral1"
      case ReactiveTypes.SignalClass => "cornflowerblue"
      case ReactiveTypes.EventClass => "aquamarine1"
      case ReactiveTypes.HandlerClass => "gold"
      case _ => "white"
    })
  }
    
  private def label(node: LogNode): String = 
    (if(node.meta.varname != NodeMetaInfo.NoVarName) "<B>" + node.meta.varname + "</B><BR/>" else "") + 
    node.typename
}


class StatisticsLogger(out: PrintStream) extends Logger(out) with LogRecorder {
  def snapshot {
    def mean[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = 
    	num.toFloat(ts.sum) / ts.size
    
    val nodes = logevents.collect{ case LogCreateNode(node) => node }
    val nNodes = nodes.size
    val attaches = logevents.collect { case LogAttachNode(node, parent) => (parent.identifier, node.identifier) }
    val nAttaches = attaches.size
    val edges = attaches.toSet
    val nEdges = edges.size
    val nChildren = edges.groupBy(_._1).mapValues(_.size).toSeq
    val nParents = edges.groupBy(_._2).mapValues(_.size).toSeq
    val averageChildren = mean(nChildren.map(_._2))
    val averageParents = mean(nParents.map(_._2))
    val nodetypes = nodes.map(_.typename)
    val nodetypeDistribution = nodetypes
    	.groupBy(identity)
    	.mapValues(_.size).toSeq
    	.sortBy(_._2).reverse
    val stamps = logevents.collect { case LogRound(stamp) => stamp }
    val rounds = stamps.map(_.sequenceNum).toSet.size
    val turns = stamps.map (s => (s.sequenceNum, s.roundNum)).toSet.size
    val averageTurns = turns.toFloat / rounds
    
    out.println("Nodes: " + nNodes)
    out.println("Edges: " + nEdges)
    out.println("Attach dependant: " + nAttaches)
    out.println("Average dependants: "+ averageChildren)
    out.println("Average depend-ons: " + averageParents)
    out.println("Total rounds: " + rounds)
    out.println("Total turns: " + turns)
    out.println("Average turns: " + averageTurns)
    out.println("Node connectivity: ")
    out.println("Total notify dependents: ")
    
    out.println("Node types: ")
    out.println(nodetypeDistribution.collect{
      case (s, c) => s + "\t" + c}.mkString("\t","\n\t",""))
  }
}
