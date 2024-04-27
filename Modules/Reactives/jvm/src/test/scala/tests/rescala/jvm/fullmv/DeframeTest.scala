package tests.rescala.fullmv

import reactives.core.{Derived, ReSource}
import reactives.fullmv.NotificationBranchResult.ReevOutBranchResult.{NotifyAndNonReadySuccessor, PureNotifyOnly}
import reactives.fullmv.tasks.{Framing, Notification, Reevaluation, SupersedeFraming}
import reactives.fullmv.{FramingBranchResult, FullMVEngine, FullMVState, NotificationBranchResult, State}
import reactives.structure.Pulse

import scala.concurrent.duration.Duration

class DeframeTest extends munit.FunSuite {
  if reactives.default.global.scheduler.isInstanceOf[FullMVEngine] then {
    import reactives.default.*

    implicit def assumeSignalsAreFullMV(sig: ReSource): ReSource.of[State] = sig.asInstanceOf

    val engine: FullMVEngine = reactives.default.global.scheduler.asInstanceOf[FullMVEngine]

    test("deframe") {

      val x = reactives.default

      val engine = new FullMVEngine(Duration.Zero, "deframe-test")
      type State[T] = reactives.fullmv.State[T]

      val dummy = Signal { -1 }

      val r      = Signal { 0 }
      val right  = r.asInstanceOf[Derived.of[State]]
      val m      = Signal { r.value + 1 }
      val middle = m.asInstanceOf[Derived.of[State]]
      val t      = Signal { m.value + 1 }
      val top    = t.asInstanceOf[Derived.of[State]]

      val turnLeftOne = engine.newTurn()
      turnLeftOne.beginFraming()
      assertEquals(new Framing(turnLeftOne, middle).doFraming(), FramingBranchResult.Frame(Set(top), turnLeftOne))
      assertEquals(new Framing(turnLeftOne, top).doFraming(), FramingBranchResult.Frame(Set(), turnLeftOne))
      turnLeftOne.completeFraming()
      assertEquals(
        new Notification(
          turnLeftOne,
          middle,
          changed = true
        ).deliverNotification(),
        true -> NotificationBranchResult.ReevaluationReady
      )

      val turnRightOne = engine.newTurn()
      turnRightOne.beginFraming()
      turnRightOne.activeBranches.incrementAndGet()
      assertEquals(new Framing(turnRightOne, dummy).doFraming(), FramingBranchResult.Frame(Set(), turnRightOne))
      val turnRightTwo = engine.newTurn()
      turnRightTwo.beginFraming()
      turnRightTwo.activeBranches.incrementAndGet()
      assertEquals(new Framing(turnRightTwo, dummy).doFraming(), FramingBranchResult.FramingBranchEnd)

      assertEquals(new Framing(turnRightTwo, right).doFraming(), FramingBranchResult.Frame(Set(middle), turnRightTwo))
      assertEquals(new Framing(turnRightTwo, middle).doFraming(), FramingBranchResult.FramingBranchEnd)

      assertEquals(
        new Framing(turnRightOne, right).doFraming(),
        FramingBranchResult.FrameSupersede(
          Set(middle),
          turnRightOne,
          turnRightTwo
        )
      )
      turnLeftOne.drop(right, middle)

      val reevMiddle = new Reevaluation(turnLeftOne, middle)
      assertEquals(
        reevMiddle.processReevaluationResult(
          Some(Pulse.Value(123).asInstanceOf[reevMiddle.node.Value])
        ),
        PureNotifyOnly(Set(top))
      )
      //    assertEquals(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])), FollowFraming(Set(top), turnRightTwo))
      assertEquals(
        new Notification(
          turnLeftOne,
          top,
          changed = true
        ).deliverNotification(),
        true -> NotificationBranchResult.ReevaluationReady
      )
      //    assertEquals(NotificationWithFollowFrame(turnLeftOne, top, changed = true, turnRightTwo).deliverNotification(), NotificationResultAction.GlitchFreeReady)

      assertEquals(
        new SupersedeFraming(
          turnRightOne,
          middle,
          turnRightTwo
        ).doFraming(),
        FramingBranchResult.FramingBranchEnd
      )
      //    assertEquals(SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming(), FramingBranchResult.Deframe(Set(top), turnRightTwo))
      //    assertEquals(Deframing(turnRightTwo, top).doFraming(), FramingBranchResult.FramingBranchEnd)

      val reevTop = new Reevaluation(turnLeftOne, top)
      assertEquals(
        reevTop.processReevaluationResult(
          Some(Pulse.Value(234).asInstanceOf[reevTop.node.Value])
        ),
        PureNotifyOnly(Set())
      )
    }

    test("deframe-reframe") {

      val dummy = Signal { -1 }

      val r      = Signal { 0 }
      val right  = r
      val m      = Signal { r.value + 1 }
      val middle = m.asInstanceOf[Derived.of[State]]
      val t      = Signal { m.value + 1 }
      val top    = t.asInstanceOf[Derived.of[State]]

      val turnLeftOne = engine.newTurn()
      turnLeftOne.beginFraming()
      assertEquals(new Framing(turnLeftOne, middle).doFraming(), FramingBranchResult.Frame(Set(top), turnLeftOne))
      assertEquals(new Framing(turnLeftOne, top).doFraming(), FramingBranchResult.Frame(Set(), turnLeftOne))
      turnLeftOne.completeFraming()
      assertEquals(
        new Notification(
          turnLeftOne,
          middle,
          changed = true
        ).deliverNotification(),
        true -> NotificationBranchResult.ReevaluationReady
      )

      val turnRightOne = engine.newTurn()
      turnRightOne.beginFraming()
      turnRightOne.activeBranches.incrementAndGet()
      assertEquals(new Framing(turnRightOne, dummy).doFraming(), FramingBranchResult.Frame(Set(), turnRightOne))
      val turnRightTwo = engine.newTurn()
      turnRightTwo.beginFraming()
      turnRightTwo.activeBranches.incrementAndGet()
      assertEquals(new Framing(turnRightTwo, dummy).doFraming(), FramingBranchResult.FramingBranchEnd)

      assertEquals(new Framing(turnRightTwo, right).doFraming(), FramingBranchResult.Frame(Set(middle), turnRightTwo))
      assertEquals(new Framing(turnRightTwo, middle).doFraming(), FramingBranchResult.FramingBranchEnd)

      val turnLeftTwo = engine.newTurn()
      turnLeftTwo.beginFraming()
      assertEquals(new Framing(turnLeftTwo, middle).doFraming(), FramingBranchResult.FramingBranchEnd)

      assertEquals(
        new Framing(turnRightOne, right).doFraming(),
        FramingBranchResult.FrameSupersede(
          Set(middle),
          turnRightOne,
          turnRightTwo
        )
      )
      turnLeftOne.drop(right, middle)

      val reevMiddle = new Reevaluation(turnLeftOne, middle)
      assertEquals(
        reevMiddle.processReevaluationResult(
          Some(Pulse.Value(123).asInstanceOf[reevMiddle.node.Value])
        ),
        PureNotifyOnly(Set(top))
      )
      //    assertEquals(reevMiddle.processReevaluationResult(Some(123.asInstanceOf[reevMiddle.node.Value])), FollowFraming(Set(top), turnRightTwo))
      assertEquals(
        new Notification(
          turnLeftOne,
          top,
          changed = true
        ).deliverNotification(),
        true -> NotificationBranchResult.ReevaluationReady
      )
      //    assertEquals(NotificationWithFollowFrame(turnLeftOne, top, changed = true, turnRightTwo).deliverNotification(), NotificationResultAction.GlitchFreeReady)

      assertEquals(
        new SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming(),
        FramingBranchResult.Frame(
          Set(top),
          turnLeftTwo
        )
      )
      //    assertEquals(SupersedeFraming(turnRightOne, middle, turnRightTwo).doFraming(), FramingBranchResult.DeframeReframe(Set(top), turnRightTwo, turnLeftTwo))
      assertEquals(new Framing(turnLeftTwo, top).doFraming(), FramingBranchResult.FramingBranchEnd)
      //    assertEquals(DeframeReframing(turnRightTwo, top, turnLeftTwo).doFraming(), FramingBranchResult.FramingBranchEnd)

      val reevTop = new Reevaluation(turnLeftOne, top)
      assertEquals(
        reevTop.processReevaluationResult(
          Some(Pulse.Value(234).asInstanceOf[reevTop.node.Value])
        ),
        NotifyAndNonReadySuccessor(Set(), turnLeftTwo)
      )
    }
  }
}
