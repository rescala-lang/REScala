package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NoSuccessor}
import rescala.fullmv.{FramingBranchResult, FullMVEngine, NotificationResultAction}
import rescala.fullmv.tasks._

import scala.concurrent.duration.Duration

class DeframeTest extends FunSuite {
  test("deframe") {
    val engine = new FullMVEngine(Duration.Zero, "deframe-test")
    import engine._

    val dummy = Signal {-1}.asInstanceOf[Reactive]

    val r = Signal { 0 }
    val right = r.asInstanceOf[Reactive]
    val m = Signal {r() + 1}
    val middle = m.asInstanceOf[Reactive]
    val t = Signal {m() + 1}
    val top = t.asInstanceOf[Reactive]

    val turnLeftOne = engine.newTurn()
    turnLeftOne.beginFraming()
    assert(new Framing(turnLeftOne, middle).doFraming() === FramingBranchResult.Frame(Set(top), turnLeftOne))
    assert(new Framing(turnLeftOne, top).doFraming() === FramingBranchResult.Frame(Set(), turnLeftOne))
    turnLeftOne.completeFraming()
    assert(new Notification(turnLeftOne, middle, changed = true).deliverNotification() === NotificationResultAction.GlitchFreeReady)

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

    assert(new Framing(turnRightOne, right).doFraming() === FramingBranchResult.FrameSupersede(Set(middle), turnRightOne, turnRightTwo))
    turnLeftOne.drop(right, middle)

    val reevMiddle = new Reevaluation(turnLeftOne, middle)
    assert(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])) === NoSuccessor(Set(top)))
    //    assert(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])) === FollowFraming(Set(top), turnRightTwo))
    assert(new Notification(turnLeftOne, top, changed = true).deliverNotification() === NotificationResultAction.GlitchFreeReady)
    //    assert(NotificationWithFollowFrame(turnLeftOne, top, changed = true, turnRightTwo).deliverNotification() === NotificationResultAction.GlitchFreeReady)

    assert(new SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.FramingBranchEnd)
    //    assert(SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.Deframe(Set(top), turnRightTwo))
    //    assert(Deframing(turnRightTwo, top).doFraming() === FramingBranchResult.FramingBranchEnd)

    val reevTop = new Reevaluation(turnLeftOne, top)
    assert(reevTop.processReevaluationResult(Some(234.asInstanceOf[reevTop.node.Value])) === NoSuccessor(Set()))
  }

  test("deframe-reframe") {
    val engine = new FullMVEngine(Duration.Zero, "deframe-reframe-test")
    import engine._

    val dummy = Signal {-1}.asInstanceOf[Reactive]

    val r = Signal { 0 }
    val right = r.asInstanceOf[Reactive]
    val m = Signal {r() + 1}
    val middle = m.asInstanceOf[Reactive]
    val t = Signal {m() + 1}
    val top = t.asInstanceOf[Reactive]

    val turnLeftOne = engine.newTurn()
    turnLeftOne.beginFraming()
    assert(new Framing(turnLeftOne, middle).doFraming() === FramingBranchResult.Frame(Set(top), turnLeftOne))
    assert(new Framing(turnLeftOne, top).doFraming() === FramingBranchResult.Frame(Set(), turnLeftOne))
    turnLeftOne.completeFraming()
    assert(new Notification(turnLeftOne, middle, changed = true).deliverNotification() === NotificationResultAction.GlitchFreeReady)

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

    assert(new Framing(turnRightOne, right).doFraming() === FramingBranchResult.FrameSupersede(Set(middle), turnRightOne, turnRightTwo))
    turnLeftOne.drop(right, middle)

    val reevMiddle = new Reevaluation(turnLeftOne, middle)
    assert(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])) === NoSuccessor(Set(top)))
    //    assert(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])) === FollowFraming(Set(top), turnRightTwo))
    assert(new Notification(turnLeftOne, top, changed = true).deliverNotification() === NotificationResultAction.GlitchFreeReady)
    //    assert(NotificationWithFollowFrame(turnLeftOne, top, changed = true, turnRightTwo).deliverNotification() === NotificationResultAction.GlitchFreeReady)

    assert(new SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.Frame(Set(top), turnLeftTwo))
    //    assert(SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming() === FramingBranchResult.DeframeReframe(Set(top), turnRightTwo, turnLeftTwo))
    assert(new Framing(turnLeftTwo, top).doFraming() === FramingBranchResult.FramingBranchEnd)
    //    assert(DeframeReframing(turnRightTwo, top, turnLeftTwo).doFraming() === FramingBranchResult.FramingBranchEnd)

    val reevTop = new Reevaluation(turnLeftOne, top)
    assert(reevTop.processReevaluationResult(Some(234.asInstanceOf[reevTop.node.Value])) === FollowFraming(Set(), turnLeftTwo))
  }
}
