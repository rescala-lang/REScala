package rescala.fullmv.api

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.{SortedMap, mutable}

class SignalVersionList[V, O <: SignalVersionList[_, _]](val sgt: SerializationGraphTracking, init: Transaction, initialValue: V) {
  sealed abstract class Version(val txn: Transaction, var out: Set[O], val performedFraming: Boolean, val performedChangedPropagation: Boolean)
  final class ReadVersion(txn: Transaction, out: Set[O]) extends Version(txn, out, false, false)
  sealed abstract class Frame(txn: Transaction, out: Set[O]) extends Version(txn, out, true, false)
  final class Pending(txn: Transaction, out: Set[O], var pending: Int = 1, var changed: Int = 0) extends Frame(txn, out)
  final class Ready(txn: Transaction, out: Set[O]) extends Frame(txn, out)
  final class Active(txn: Transaction, out: Set[O]) extends Frame(txn, out)
  final class Written(txn: Transaction, out: Set[O], val value: V) extends Version(txn, out, true, true)

  var _versions = Array[Version](new Written(init, Set(), initialValue))

  def ensureOrdered(txn: Transaction): SearchResult = {
    ensureOrdered(txn, InsertionPoint(_versions.size))
    @tailrec
    def ensureOrdered(txn: Transaction, attempt: InsertionPoint): SearchResult = {
      if(sgt.requireOrder(_versions(attempt.insertionPoint - 1).txn, txn) == FirstFirst) {
        attempt
      } else {
        import scala.collection.Searching._
        object SearchOrdering extends Ordering[Transaction] {
          override def compare(x: Transaction, y: Transaction): Int = {
            if (x == y){
              0
            } else {
              sgt.getOrder(x, y) match {
                case Some(FirstFirst) => -1
                case Some(SecondFirst) => 1
                case None => -1
              }
            }
          }
        }
        _versions.search(txn) match {
          case found @ Found(position) => found
          case attempt @ InsertionPoint(position) => ensureOrdered(txn, attempt)
          // TODO could optimize this case by not restarting with entire array bounds, but need to track fixedMin and min separately, following both above -1 cases.
        }
      }
    }
  }

//  def versions(): SortedMap[Transaction, Version] = _versions
//  def before(txn: Transaction): SortedMap[Transaction, Version] = versions().until(txn)
//  def after(txn: Transaction): SortedMap[Transaction, Version] = versions().from(txn).tail
//  def reevs(versions: SortedMap[Transaction, Version]): SortedMap[Transaction, Version] = versions.filter(_._2.performedFraming)
//  def last(versions: SortedMap[Transaction, Version]): Version = versions.last._2
//  def first(versions: SortedMap[Transaction, Version]): Option[Version] = versions.headOption.map(_._2)

  def prevReevPosition(from: Int): Int = {
    var prevReevPosition = from - 1
    while(prevReevPosition >= 0 && !_versions(prevReevPosition).performedFraming) prevReevPosition -= 1
    if(prevReevPosition < 0) throw new IllegalArgumentException("Does not have a preceding Reevaluation: "+_versions(from))
    prevReevPosition
  }

  def nextReevPosition(from: Int): Option[Int] = {
    var nextReevPosition = from + 1
    while(nextReevPosition < _versions.size && !_versions(nextReevPosition).performedFraming) nextReevPosition += 1
    if(nextReevPosition < _versions.size) {
      Some(nextReevPosition)
    } else {
      None
    }
  }

  def incrementFrame(txn: Transaction): Set[O] = {
    def createPending(position: Int, out: Set[O]): Pending = {
      val pending = new Pending(txn, out)
      if(_versions(prevReevPosition(position)).performedChangedPropagation) pending.pending += 1
      pending
    }

    ensureOrdered(txn) match {
      case Found(position) =>
        _versions(position) match {
          case frame: Pending =>
            frame.pending += 1
            Set.empty
          case read: ReadVersion =>
            _versions(position) = createPending(position, read.out)
            read.out
          case version =>
            throw new IllegalStateException("Cannot incrementFrame: not a Frame or ReadVersion: " + version)
        }
      case InsertionPoint(position) =>
        val out = _versions(position - 1).out
        createPending(position, out)
        out
    }
  }

  def changed(txn: Transaction): Option[Transaction] = {
    ensureOrdered(txn) match {
      case Found(position) =>
        _versions(position) match {
          case frame: Pending if (frame.pending > 0) =>
            frame.pending -= 1
            frame.changed += 1
            if (frame.pending == 0) {
              _versions(position) = new Ready(txn, frame.out)
              Some(txn)
            } else {
              None
            }
          case version =>
            throw new IllegalStateException("Cannot process change notification - not a Frame: " + version)
        }
      case _ =>
        throw new IllegalStateException("Cannot process change notification - no Frame for " + txn)
    }
  }

//  @tailrec
  def unchanged(position: Int): Option[Transaction] = {
    _versions(position) match {
      case frame: Pending if (frame.pending > 0) =>
        frame.pending -= 1
        if (frame.pending == 0) {
          if (frame.changed > 0) {
            _versions(position) = new Ready(frame.txn, frame.out)
            Some(frame.txn)
          } else {
            _versions(position) = new ReadVersion(frame.txn, frame.out)
            nextReevPosition(position).flatMap(unchanged(_))
//            nextReevPosition(position) match {
//              case Some(next) => unchanged(next)
//              case None => None
//            }
          }
        } else {
          None
        }
      case version =>
        throw new IllegalStateException("Cannot process no-change notification - not a Frame: " + version)
    }
  }

  def unchanged(txn: Transaction): Option[Transaction] = {
    ensureOrdered(txn) match {
      case Found(position) =>
        unchanged(position)
      case _ =>
        throw new IllegalStateException("Cannot process no-change notification - no Frame for " + txn)
    }
  }
  def dechange(txn: Transaction): Unit = {
    ensureOrdered(txn) match {
      case Found(position) =>
        _versions(position) match {
          case frame: Pending if (frame.pending > 0) =>
            frame.changed -= 1
          case version =>
            throw new IllegalStateException("Cannot process de-change notification - not a Frame: " + version)
        }
      case _ =>
        throw new IllegalStateException("Cannot process de-change notification - no Frame for " + txn)
    }
  }

  def reevIn(txn: Transaction): V = {
    ensureOrdered(txn) match {
      case Found(position) =>
        _versions(position) match {
          case frame: Ready =>
            _versions(prevReevPosition(position)) match {
              case written: Written =>
                _versions(position) = new Active(txn, frame.out)
                written.value
              case version =>
                throw new IllegalStateException("Cannot reevIn - incomplete predecessor reevaluation for " + txn + ": " + version)
            }
          case version =>
            throw new IllegalStateException("Cannot reevIn - not Ready: " + version)
        }
      case _ =>
        throw new IllegalStateException("Cannot reevIn - no Frame for " + txn)
    }
  }

  def reevOut(txn: Transaction, value: V): (Set[(O, Transaction)], Option[Transaction]) = {
    ensureOrdered(txn) match {
      case Found(position) =>
        _versions(position) match {
          case frame: Active =>
            _versions(prevReevPosition(position)) match {
              case predecessor: Written =>
                val newReadyReevaluations = if (predecessor.value == value) {
                  _versions(position) = new ReadVersion(txn, frame.out)
                  frame.out.flatMap(o => o.unchanged(txn).map(o -> _))
                } else {
                  _versions(position) = new Written(txn, frame.out, value)
                  frame.out.flatMap(o => o.unchanged(txn).map(o -> _))
                }
                (newReadyReevaluations, nextReevPosition(position).flatMap(unchanged(_))
            )
              case version =>
                throw new IllegalStateException("Cannot reevOut - incomplete predecessor reevaluation (must have been established after reevIn was performed!) for " + txn + ": " + version)
            }
          case version =>
            throw new IllegalStateException("Cannot reevOut - not Active: " + version)
        }
      case _ =>
        throw new IllegalStateException("Cannot reevOut - no Frame for: " + txn)
    }
  }
}
