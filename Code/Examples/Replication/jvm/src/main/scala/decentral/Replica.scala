package decentral

import decentral.Bindings._
import kofre.base.DecomposeLattice
import kofre.datatypes.AddWinsSet
import kofre.decompose.containers.DeltaBufferRDT
import kofre.dotted.Dotted
import kofre.syntax.DottedName
import loci.transmitter.{RemoteAccessException, RemoteRef}

import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.matching.Regex

class Replica(val listenPort: Int, val connectTo: List[(String, Int)], id: String, initSize: Int) extends Peer {
  val add: Regex       = """add (\d+)""".r
  val remove: Regex    = """remove (\d+)""".r
  val clear: String    = "clear"
  val elements: String = "elements"
  val size: String     = "size"
  val exit: String     = "exit"

  val minAtomsForCheckpoint = 100
  val maxAtomsForCheckpoint = 500

  var set: DeltaBufferRDT[AddWinsSet[Int]] = DeltaBufferRDT.empty(id, AddWinsSet.empty)

  var checkpoints: Map[String, Int] = Map(id -> 0)

  var checkpointMap: Map[Checkpoint, SetState] = Map()

  var unboundLocalChanges: List[SetState] = List()

  var unboundRemoteChanges: SetState = Dotted(AddWinsSet.empty[Int])

  def sendDeltaRecursive(
      remoteReceiveDelta: SetState => Future[Unit],
      atoms: Iterable[SetState],
      merged: SetState
  ): Unit = {
    remoteReceiveDelta(merged).failed.foreach {
      case e: RemoteAccessException => e.reason match {
          case RemoteAccessException.RemoteException(name, _) if name.contains("JsonReaderException") =>
            val (firstHalf, secondHalf) = {
              val a =
                if (atoms.isEmpty) DecomposeLattice[SetState].decompose(merged)
                else atoms

              val atomsSize = a.size

              (a.take(atomsSize / 2), a.drop(atomsSize / 2))
            }

            sendDeltaRecursive(remoteReceiveDelta, firstHalf, firstHalf.reduce(DecomposeLattice[SetState].merge))
            sendDeltaRecursive(remoteReceiveDelta, secondHalf, secondHalf.reduce(DecomposeLattice[SetState].merge))
          case _ => e.printStackTrace()
        }

      case e => e.printStackTrace()
    }
  }

  def sendDelta(deltaState: SetState, rr: RemoteRef): Unit =
    sendDeltaRecursive(registry.lookup(receiveDeltaBinding, rr), List(), deltaState)

  def propagateDeltas(): Unit = {
    registry.remotes.foreach { rr =>
      set.deltaBuffer.collect {
        case DottedName(replicaID, deltaState) if replicaID != rr.toString => deltaState
      }.reduceOption(DecomposeLattice[SetState].merge).foreach(sendDelta(_, rr))
    }

    set = set.resetDeltaBuffer()
  }

  def bindGetCheckpoints(): Unit = registry.bind(getCheckpointsBinding) { () => checkpoints }

  def bindReceiveDelta(): Unit = registry.bindSbj(receiveDeltaBinding) { (remoteRef: RemoteRef, deltaState: SetState) =>
    val delta = DottedName(remoteRef.toString, deltaState)
    set = set.applyDelta(delta)

    set.deltaBuffer.headOption match {
      case None =>
      case Some(DottedName(_, deltaState)) =>
        unboundRemoteChanges = DecomposeLattice[SetState].merge(unboundRemoteChanges, deltaState)

        propagateDeltas()

        println(set.elements)
    }
  }

  def bindReceiveCheckpoint(): Unit =
    registry.bindSbj(receiveCheckpointBinding) { (remoteRef: RemoteRef, msg: CheckpointMessage) =>
      msg match {
        case CheckpointMessage(cp @ Checkpoint(replicaID, counter), changes) =>
          if (checkpoints.contains(replicaID) && checkpoints(replicaID) >= counter) ()
          else {
            set = set.applyDelta(DottedName(remoteRef.toString, changes)).resetDeltaBuffer()

            unboundRemoteChanges =
              DecomposeLattice[SetState].diff(changes, unboundRemoteChanges).getOrElse(Dotted(AddWinsSet.empty))

            checkpoints = checkpoints.updated(replicaID, counter)

            checkpointMap = checkpointMap.updated(cp, changes)

            registry.remotes.foreach { rr =>
              if (rr != remoteRef) sendCheckpoint(msg, rr)
            }
          }
      }
    }

  def sendCheckpoint(msg: CheckpointMessage, rr: RemoteRef): Unit = {
    val receiveCheckpoint = registry.lookup(receiveCheckpointBinding, rr)
    receiveCheckpoint(msg)
    ()
  }

  def createCheckpoint(atoms: List[SetState]): Unit = {
    val newCounter = checkpoints(id) + 1
    checkpoints = checkpoints.updated(id, newCounter)

    val newCheckpoint = Checkpoint(id, newCounter)
    val changes       = atoms.reduce(DecomposeLattice[SetState].merge)
    checkpointMap = checkpointMap.updated(newCheckpoint, changes)

    registry.remotes.foreach { sendCheckpoint(CheckpointMessage(newCheckpoint, changes), _) }
  }

  def createCheckpoints(): Unit = {
    while (unboundLocalChanges.size > maxAtomsForCheckpoint) {
      val changesForCheckPoint = unboundLocalChanges.takeRight(maxAtomsForCheckpoint)
      unboundLocalChanges = unboundLocalChanges.dropRight(maxAtomsForCheckpoint)
      createCheckpoint(changesForCheckPoint)
    }

    if (unboundLocalChanges.size >= minAtomsForCheckpoint) {
      createCheckpoint(unboundLocalChanges)
      unboundLocalChanges = List()
    }
  }

  def onMutate(): Unit = {
    if (set.deltaBuffer.isEmpty) {
      return
    }

    unboundLocalChanges = set.deltaBuffer.foldLeft(unboundLocalChanges) { (list, delta) =>
      list.prependedAll(DecomposeLattice[SetState].decompose(delta.anon))
    }

    if (unboundLocalChanges.size < minAtomsForCheckpoint) {
      propagateDeltas()
    } else {
      createCheckpoints()

      if (unboundLocalChanges.nonEmpty) {
        val delta = unboundLocalChanges.reduce(DecomposeLattice[SetState].merge)

        registry.remotes.foreach { sendDelta(delta, _) }
      }

      set = set.resetDeltaBuffer()
    }
  }

  def monitorJoin(): Unit = {
    registry.remoteJoined.monitor { rr =>
      registry.lookup(getCheckpointsBinding, rr)().map { remoteCheckpoints =>
        checkpoints.foreachEntry { (replicaID, counter) =>
          val remoteCounter = remoteCheckpoints.getOrElse(replicaID, 0)

          (remoteCounter + 1 to counter).foreach { n =>
            val cp      = Checkpoint(replicaID, n)
            val changes = checkpointMap(cp)

            sendCheckpoint(CheckpointMessage(cp, changes), rr)
          }
        }
      }

      val unboundChanges = unboundLocalChanges.foldLeft(unboundRemoteChanges) { DecomposeLattice[SetState].merge }

      if (unboundChanges != Dotted(AddWinsSet.empty))
        sendDelta(unboundChanges, rr)
    }
    ()
  }

  def run(): Unit = {
    bindGetCheckpoints()
    bindReceiveDelta()
    bindReceiveCheckpoint()

    monitorJoin()

    setupConnectionHandling()

    set = set.addAll(0 until initSize)
    onMutate()

    while (true) {
      readLine() match {
        case add(n) =>
          set = set.add(n.toInt)
          onMutate()

        case remove(n) =>
          set = set.remove(n.toInt)
          onMutate()

        case `clear` =>
          set = set.clear()
          onMutate()

        case `elements` =>
          println(set.elements)

        case `size` =>
          println(set.elements.size)

        case `exit` =>
          System.exit(0)

        case _ => println("Unknown command")
      }
    }
  }
}
