package react.log

import react._
import scala.reflect.runtime.universe._
import java.io.PrintStream
import react.events.Event
import java.io.File
import react.events.EventHandler
import react.events.ImperativeEvent
import react.ReactiveEngine

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

// -- Everything below here should be moved out of the project --

/** A Logger that records events */
trait LogRecorder extends Logger {
  val logevents: Iterable[LogEvent]
}


/** A Logger which prints to some output stream */
trait PrintStreamLogger {
  val out: PrintStream
  
  //prevents accidently printing to System.out
  def print(x: Any) = out.print(x)
  def println(x: Any) = out.println(x)
}

/** A simple logger which prints all events to the output stream */
class TextLogger(val out: PrintStream) extends Logger with PrintStreamLogger {
  def log(logevent: LogEvent) = println(logevent)
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


object MyLogging {
  val DefaultPathPrefix = "./logs/"
  val DefaultSourceFolder = "./src/"
}

class MyLogging extends Logging {
  implicit val self = this
  
  def node(reactive: Reactive): LogNode = LogNodeImpl(reactive)
  
  val loggers = new scala.collection.mutable.MutableList[Logger]

  def addLogger(logger: => Logger) = { loggers += logger }

  def log(logevent: LogEvent) = loggers foreach (_ log logevent)
  
  lazy val logrecorder = {
    val recorder = new LogRecorder {
        val logevents = new scala.collection.mutable.ListBuffer[LogEvent]
        def log(logevent: LogEvent) = logevents += logevent
    }
    // recorder is only added once
    this addLogger recorder
    recorder
  }
}


/** enables all Logging for the MyLogging class
 *   Instanciate: new MyLogging with AllLoggingEnabled */
trait AllLoggingEnabled { self: MyLogging =>
    
  val maybeName = for (
      main <- Thread.getAllStackTraces.keySet().toArray().find(_.asInstanceOf[Thread].getName == "main")
    ) yield main.asInstanceOf[Thread].getStackTrace.last.getFileName
    val name = maybeName.getOrElse("LOG").takeWhile(_ != '.')

    val dotGraphLogger = new react.log.DotGraphLogger(
      new java.io.PrintStream(
        new java.io.FileOutputStream(
            MyLogging.DefaultPathPrefix + name + ".dot", false)))
     
    val dotHeatLogger = new react.log.DotGraphLogger(
      new java.io.PrintStream(
        new java.io.FileOutputStream(
            MyLogging.DefaultPathPrefix + name + "_heat.dot", false)),
        drawHeatmap = true)

    val reactplayerLogger = new react.log.ReactPlayerLog(
      new java.io.PrintStream(
        new java.io.FileOutputStream(
            MyLogging.DefaultPathPrefix + name + ".txt", false)))

    val statslogger = new StatisticsLogger(
      new java.io.PrintStream(
        new java.io.FileOutputStream(
            MyLogging.DefaultPathPrefix + name + "_stats.yaml", false)));

    addLogger(reactplayerLogger)
    addLogger(dotGraphLogger)
    addLogger(dotHeatLogger)
    addLogger(statslogger)  
}


/** All RecordedLoggers share the same logrecoder in the implicit logging class */
abstract class RecordedLogger(implicit val logging: MyLogging) extends Logger {
  
  val logevents: Iterable[LogEvent] = logging.logrecorder.logevents
  def log(logevent: LogEvent) = ()

  def snapshot

  /** Performs a snapshot the first time this is called, subsequent calls are ignored */
  def snapshotOnce {
    if (snapshotDone) return
    snapshotDone = true
    snapshot
  }
  private var snapshotDone = false

  // If the snapshot was never called manually, snapshot on program exit
  scala.sys.addShutdownHook { snapshotOnce }
}



case class LogNodeImpl(reactive: Reactive) extends LogNode(reactive) {
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

    if (ReactiveTypes.VarClass.isInstance(r)) {
      val thistype = if (primitive) "Var" else typename
      val value = r.asInstanceOf[Var[_]].getValue
      return thistype + "[" + getInner(value) + "]"
    } else if (ReactiveTypes.SignalClass.isInstance(r)) {
      val thistype = if (primitive) "Signal" else typename
      val value = r.asInstanceOf[Signal[_]].getValue
      return thistype + "[" + getInner(value) + "]"
    } else if (r.isInstanceOf[Event[_]]) {
      return typename
    }

    def getInner(value: Any) =
      if (value == null) "?" else if (value.isInstanceOf[Reactive]) getParametricType(value.asInstanceOf[Reactive])
      else value.getClass.getSimpleName

    return reactive.getClass.getSimpleName // should not be reached
  }
}



class ReactPlayerLog(val out: PrintStream) extends Logger with PrintStreamLogger {
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
class DotGraphLogger(out: PrintStream, drawHeatmap: Boolean = false)
  (override implicit val logging: MyLogging)
  extends RecordedLogger {
  
  private val style = if(drawHeatmap) heatmapStyle _ else normalStyle _

  def snapshot {
    if (logevents.isEmpty) return

    val existingLinks = new collection.mutable.HashSet[(Int, Int)]
    def doIfLinkNotPresent(fromTo: (Int, Int))(body: => Unit) {
      if (!existingLinks.contains(fromTo)) {
        existingLinks.add(fromTo)
        existingLinks.add(fromTo.swap)
        body
      }
    }

    out.println("digraph G {")
    out.println("node [shape=box]")
    for (e <- logevents) {
      e match {
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
      }
    }
    out.println("}")
    //clear
  }

  private lazy val timesPerNode = StatisticsLogger.timePerNode(logevents)
  private lazy val maxTimePerNode = timesPerNode.values.max.toFloat
  private def heatmapStyle(node: LogNode): String = {
	  def interpolatedHEX(a: Float): String = {
        val intcolor = java.awt.Color.getHSBColor(a * (120f / 360f), 0.75f, 1).getRGB
        "#%06X".format(0xFFFFFF & intcolor);
      }
      val time = timesPerNode.getOrElse(node.identifier, 0l) / maxTimePerNode
      "style=filled fillcolor=\"%s\"".format(interpolatedHEX(1 - time))   
  }  
  private def normalStyle(node: LogNode): String = {
      "style=filled fillcolor=\"%s\"".format(node.simpletype match {
        case ReactiveTypes.ImperativeClass => "brown1"
        case ReactiveTypes.VarClass => "coral1"
        case ReactiveTypes.SignalClass => "cornflowerblue"
        case ReactiveTypes.EventClass => "aquamarine1"
        case ReactiveTypes.HandlerClass => "gold"
        case _ => "white"
      })
    
  }

  private def label(node: LogNode): String =
    (if (node.meta.varname != NodeMetaInfo.NoVarName) "<B>" + node.meta.varname + "</B><BR/>" else "") +
      node.typename
}

object StatisticsLogger {

  /**
   * Given a list of log events, computes the time spend on each reactive node
   */
  def timePerNode(events: Iterable[LogEvent]): Map[Int, Long] = {
    val startEnd = events.collect {
      case start @ LogStartEvalNode(node) => Left(node.identifier, start.time)
      case end @ LogEndEvalNode(node) => Right(node.identifier, end.time)
    }.toList

    timePerNodeImpl(startEnd)
  }

  private def timePerNodeImpl(
      startEnd: List[Either[(Int, Long), (Int, Long)]],
      countTimesInstead: Boolean = false): Map[Int, Long] = {

    def sum(map1: Map[Int, Long], map2: Map[Int, Long]): Map[Int, Long] =
      map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0l)) }

    startEnd match {
      case Nil => Map()

      case Left((node, start)) :: Right((node2, end)) :: xs if node == node2 =>
        sum(Map(node -> (end - start)), timePerNodeImpl(xs))

      case Left((node, start)) :: xs => 
        // Find matching Right (EndEval), ugly code follows
        val rightIndex: Either[Int, Int] = xs.zipWithIndex.foldLeft(Left(1): Either[Int, Int]) { (acc, e) =>
          (acc, e) match {
            case (Right(_), _) => acc
            case (Left(left), (Left((node1, _)), index)) if node1 == node =>
              Left(left + 1)
            case (Left(left), (Right((node1, _)), index)) if node1 == node =>
              val remain = left - 1
              if (remain == 0) Right(index) else Left(remain)
            case _ => acc
          }
        }
        rightIndex match {
          case Right(rightIndex) =>

            val inner = xs.take(rightIndex)
            val remain = xs.drop(rightIndex + 1)
            xs(rightIndex) match {
              case Right((_, endtime)) =>
                val innerTime = timePerNodeImpl(inner)
                val restTime = timePerNodeImpl(remain)

                val thisTime = (endtime - start) - innerTime.values.sum
                val hereTime = Map(node -> thisTime)
                // merge all times
                sum(sum(innerTime, restTime), hereTime)
              case _ => throw new RuntimeException("Should not occur")
            }
          case _ => Map() // throw new RuntimeException("No closing EndEval")
        }
      case _ => Map() // throw new Exception("No opening StartEval")
    }
  }
}

class StatisticsLogger(out: PrintStream)
 (override implicit val logging: MyLogging) extends RecordedLogger {
  def snapshot {
    def mean[T](ts: Iterable[T])(implicit num: Numeric[T]) =
      num.toFloat(ts.sum) / ts.size

    val nodes = logevents.collect { case LogCreateNode(node) => node }
    val varnames = nodes.map(n => (n.identifier, n.meta.varname)).toMap
    val nNodes = nodes.size
    val attaches = logevents.collect { case LogAttachNode(node, parent) => (parent.identifier, node.identifier) }
    val nAttaches = attaches.size
    val edges = attaches.toSet
    val nEdges = edges.size
    val nChildren = edges.groupBy(_._1).mapValues(_.size).toSeq
    val nParents = edges.groupBy(_._2).mapValues(_.size).toSeq
    val pulses = logevents.collect { case l @ LogPulseNode(_) => l }
    val nPulses = pulses.size
    val averageChildren = mean(nChildren.map(_._2))
    val averageParents = mean(nParents.map(_._2))
    val nodetypes = nodes.map(_.typename)
    val nodetypeDistribution = nodetypes
      .groupBy(identity)
      .mapValues(_.size).toSeq
      .sortBy(_._2).reverse
    val turns = logevents.collect { case l @ LogRound(_) => l }
    val totalTime = if (turns.isEmpty) 0 else turns.last.time - turns.head.time
    val totalSeconds = totalTime.toFloat / 1000
    val logeventsPerSecond = logevents.size / totalSeconds
    val stamps = turns.map(_.stamp)
    val rounds = turns.foldLeft((Nil: List[LogRound], 0)) { (accum, e: LogEvent) =>
      val (rounds, latest) = accum
      e match {
        case round @ LogRound(Stamp(r, _)) if r > latest => (round :: rounds, r)
        case _ => accum
      }
    }._1.reverse
    val nRounds = rounds.size
    val turnsPerRound = stamps.groupBy(_.roundNum).mapValues(_.size).toList.sorted.map(_._2)
    val nTurns = stamps.size
    val averageTurns = nTurns.toFloat / nRounds
    val millisPerRound = rounds.map(_.time).sliding(2).map(x => x.reverse.reduce(_ - _))

    val nNodesOverRounds: List[Int] = logevents.foldLeft((List(0), 0)) { (accum, e: LogEvent) =>
      val (stats, latest) = accum
      e match {
        case LogRound(Stamp(round, _)) if round > latest => (0 :: stats, round)
        case LogCreateNode(_) => ((stats.head + 1) :: stats.tail, latest)
        case _ => accum
      }
    }._1.reverse
    val nNodesOverTurns: List[Int] = logevents.foldLeft(List(0)) { (stats, e: LogEvent) =>
      e match {
        case LogRound(_) => 0 :: stats
        case LogCreateNode(_) => (stats.head + 1) :: stats.tail
        case _ => stats
      }
    }.reverse

    val nNodesOverRoundsCum = nNodesOverRounds.scan(0)(_ + _)
    val nPulseOverRounds: List[Int] = logevents.foldLeft((List(0), 0)) { (accum, e: LogEvent) =>
      val (stats, latest) = accum
      e match {
        case LogRound(Stamp(round, _)) if round > latest => (0 :: stats, round)
        case LogPulseNode(_) => ((stats.head + 1) :: stats.tail, latest)
        case _ => accum
      }
    }._1.reverse

    val startEndEval = logevents.collect {
      case start @ LogStartEvalNode(node) => Left(node.identifier, start.time)
      case end @ LogEndEvalNode(node) => Right(node.identifier, end.time)
    }.toList

    val timePerNode = StatisticsLogger.timePerNode(logevents)
      .toSeq
      .map { case (k, v) => ("%d [%s]".format(k, varnames.getOrElse(k, "?")), v.toInt) }
      .sortBy(-_._2)

    out.println("# REScala stats (this is YAML)")
    out.println("Total log events: " + logevents.size)
    out.println("Total running time: " + totalSeconds + " sec")
    out.println("Nodes: " + nNodes)
    out.println("Edges: " + nEdges)
    out.println("Attaches: " + nAttaches)
    out.println("Total rounds: " + nRounds)
    out.println("Total turns: " + nTurns)
    out.println("Total pulses: " + nPulses)
    out.println("Average dependants: " + averageChildren)
    out.println("Average depend-ons: " + averageParents)
    out.println("Average attaches per edge per round: " + nAttaches.toFloat / nEdges / nRounds)
    out.println("Average turns per round: " + averageTurns)
    out.println("Average log events per second: " + logeventsPerSecond)
    out.println("Turns per round: " + turnsPerRound.mkString("[", ", ", "]"))
    out.println("Time per round: " + millisPerRound.mkString("[", ", ", "]"))
    out.println("Created nodes per round: " + nNodesOverRounds.mkString("[", ", ", "]"))
    out.println("Cumulative nodes per round: " + nNodesOverRoundsCum.mkString("[", ", ", "]"))
    out.println("Pulses per round: " + nPulseOverRounds.mkString("[", ", ", "]"))
    //out.println("Node connectivity: ")
    out.println("Node types: ")
    out.println(nodetypeDistribution.collect {
      case (s, c) => s + ": " + c
    }.mkString("  ", "\n  ", ""))

    out.println("Time per node: ")
    out.println(timePerNode.collect {
      case (s, c) => s + ": " + c
    }.mkString("  ", "\n  ", ""))

  }
}
