package tests.rescala.fullmv

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv.NotificationBranchResult.ReevOutBranchResult.{NotifyAndNonReadySuccessor, PureNotifyOnly}
import rescala.fullmv.{FramingBranchResult, FullMVApi, NotificationBranchResult}
import rescala.core.Derived
import rescala.structures.Pulse

import scala.concurrent.duration.Duration

class DeframeTest extends AnyFunSuite {
  test("deframe") {
    object FullMVTest extends FullMVApi(Duration.Zero, "deframe-test")
    import FullMVTest._
    val engine = FullMVTest.scheduler

    val dummy = Signal { -1 }

    val r      = Signal { 0 }
    val right  = r.asInstanceOf[Derived.of[State]]
    val m      = Signal { r.value + 1 }
    val middle = m.asInstanceOf[Derived.of[State]]
    val t      = Signal { m.value + 1 }
    val top    = t.asInstanceOf[Derived.of[State]]

    val turnLeftOne = engine.newTurn()
    turnLeftOne.beginFraming()
    assert(new Framing(turnLeftOne, middle).doFraming() === FramingBranchResult.Frame(Set(top), turnLeftOne))
    assert(new Framing(turnLeftOne, top).doFraming() === FramingBranchResult.Frame(Set(), turnLeftOne))
    turnLeftOne.completeFraming()
    assert(new Notification(
      turnLeftOne,
      middle,
      changed = true
    ).deliverNotification() === true -> NotificationBranchResult.ReevaluationReady)

    val turnRightOne = engine.newTurn()
    turnRightOne.beginFraming()
    turnRightOne.activeBranches.incrementAndGet()
    assert(new Framing(turnRightOne, dummy).doFraming() === FramingBranchResult.Frame(Set(), turnRightOne))
    val turnRightTwo = engine.newTurn()
    turnRightTwo.beginFraming()
    turnRightTwo.activeBranches.incrementAndGet()
    assert(new Framing(turnRightTwo, dummy).doFraming() === FramingBranchResult.FramingBranchEnd)

    assert(new Framing(turnRightTwo, right).doFraming() === FramingBranchResult.Frame(Set(middle), turnRightTwo))
    assert(new Framing(turnRightTwo, middle).doFraming() === FramingBranchResult.FramingBranchEnd)

    assert(new Framing(turnRightOne, right).doFraming() === FramingBranchResult.FrameSupersede(
      Set(middle),
      turnRightOne,
      turnRightTwo
    ))
    turnLeftOne.drop(right, middle)

    val reevMiddle = new Reevaluation(turnLeftOne, middle)
    assert(reevMiddle.processReevaluationResult(
      Some(Pulse.Value(123).asInstanceOf[reevMiddle.node.Value])
    ) === PureNotifyOnly(Set(top)))
    //    assert(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])) === FollowFraming(Set(top), turnRightTwo))
    assert(new Notification(
      turnLeftOne,
      top,
      changed = true
    ).deliverNotification() === true -> NotificationBranchResult.ReevaluationReady)
    //    assert(NotificationWithFollowFrame(turnLeftOne, top, changed = true, turnRightTwo).deliverNotification() === NotificationResultAction.GlitchFreeReady)

    assert(new SupersedeFraming(
      turnRightOne,
      middle,
      turnRightTwo
    ).doFraming() === FramingBranchResult.FramingBranchEnd)
    //    assert(SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.Deframe(Set(top), turnRightTwo))
    //    assert(Deframing(turnRightTwo, top).doFraming() === FramingBranchResult.FramingBranchEnd)

    val reevTop = new Reevaluation(turnLeftOne, top)
    assert(reevTop.processReevaluationResult(
      Some(Pulse.Value(234).asInstanceOf[reevTop.node.Value])
    ) === PureNotifyOnly(Set()))
  }

  test("deframe-reframe") {
    object FullMVTest extends FullMVApi(Duration.Zero, "deframe-reframe-test")
    import FullMVTest._
    val engine = FullMVTest.scheduler

    val dummy = Signal { -1 }

    val r      = Signal { 0 }
    val right  = r
    val m      = Signal { r.value + 1 }
    val middle = m.asInstanceOf[Derived.of[State]]
    val t      = Signal { m.value + 1 }
    val top    = t.asInstanceOf[Derived.of[State]]

    val turnLeftOne = engine.newTurn()
    turnLeftOne.beginFraming()
    assert(new Framing(turnLeftOne, middle).doFraming() === FramingBranchResult.Frame(Set(top), turnLeftOne))
    assert(new Framing(turnLeftOne, top).doFraming() === FramingBranchResult.Frame(Set(), turnLeftOne))
    turnLeftOne.completeFraming()
    assert(new Notification(
      turnLeftOne,
      middle,
      changed = true
    ).deliverNotification() === true -> NotificationBranchResult.ReevaluationReady)

    val turnRightOne = engine.newTurn()
    turnRightOne.beginFraming()
    turnRightOne.activeBranches.incrementAndGet()
    assert(new Framing(turnRightOne, dummy).doFraming() === FramingBranchResult.Frame(Set(), turnRightOne))
    val turnRightTwo = engine.newTurn()
    turnRightTwo.beginFraming()
    turnRightTwo.activeBranches.incrementAndGet()
    assert(new Framing(turnRightTwo, dummy).doFraming() === FramingBranchResult.FramingBranchEnd)

    assert(new Framing(turnRightTwo, right).doFraming() === FramingBranchResult.Frame(Set(middle), turnRightTwo))
    assert(new Framing(turnRightTwo, middle).doFraming() === FramingBranchResult.FramingBranchEnd)

    val turnLeftTwo = engine.newTurn()
    turnLeftTwo.beginFraming()
    assert(new Framing(turnLeftTwo, middle).doFraming() === FramingBranchResult.FramingBranchEnd)

    assert(new Framing(turnRightOne, right).doFraming() === FramingBranchResult.FrameSupersede(
      Set(middle),
      turnRightOne,
      turnRightTwo
    ))
    turnLeftOne.drop(right, middle)

    val reevMiddle = new Reevaluation(turnLeftOne, middle)
    assert(reevMiddle.processReevaluationResult(
      Some(Pulse.Value(123).asInstanceOf[reevMiddle.node.Value])
    ) === PureNotifyOnly(Set(top)))
    //    assert(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])) === FollowFraming(Set(top), turnRightTwo))
    assert(new Notification(
      turnLeftOne,
      top,
      changed = true
    ).deliverNotification() === true -> NotificationBranchResult.ReevaluationReady)
    //    assert(NotificationWithFollowFrame(turnLeftOne, top, changed = true, turnRightTwo).deliverNotification() === NotificationResultAction.GlitchFreeReady)

    assert(new SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.Frame(
      Set(top),
      turnLeftTwo
    ))
    //    assert(SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.DeframeReframe(Set(top), turnRightTwo, turnLeftTwo))
    assert(new Framing(turnLeftTwo, top).doFraming() === FramingBranchResult.FramingBranchEnd)
    //    assert(DeframeReframing(turnRightTwo, top, turnLeftTwo).doFraming() === FramingBranchResult.FramingBranchEnd)

    val reevTop = new Reevaluation(turnLeftOne, top)
    assert(reevTop.processReevaluationResult(
      Some(Pulse.Value(234).asInstanceOf[reevTop.node.Value])
    ) === NotifyAndNonReadySuccessor(Set(), turnLeftTwo))
  }
}
