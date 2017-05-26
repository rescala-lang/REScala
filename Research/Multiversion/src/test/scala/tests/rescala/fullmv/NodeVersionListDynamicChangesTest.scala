//package tests.rescala.fullmv
//
//import org.scalatest.{FlatSpec, Matchers}
//import rescala.fullmv.api._
//import tests.rescala.fullmv.testutils.{TestHost, UserComputationTracker}
//
//
//class NodeVersionListDynamicChangesTest extends FlatSpec with Matchers {
//  import tests.rescala.fullmv.testutils.VersionListAsserter._
//  "A dynamic discovery" should "reincrement frames as needed" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
//    val t_discover = Transaction().branches(1)
//    val y = new SignalVersionList(TestHost, t, t, {
//      (ticket: ReevaluationTicket, v_in: Transaction) =>
//        if(ticket.txn == t_discover) x.discoverSuspend(ticket)
//        tracker.comp(ticket, v_in)
//    })
//    t.done()
//
//    val t_pred_nochange = Transaction().branches(1)
//    x.incrementFrame(t_pred_nochange)
//    t_pred_nochange.start().branches(1)
//    x.notify(t_pred_nochange, false, None)
//
//    val t_pred_change = Transaction().branches(1)
//    x.incrementFrame(t_pred_change)
//    t_pred_change.start().branches(1)
//    x.notify(t_pred_change, true, None)
//
//    y.incrementFrame(t_discover)
//    t_discover.start()
//
//    val t_succ_change_x = Transaction().branches(1)
//    x.incrementFrame(t_succ_change_x)
//    t_succ_change_x.start().branches(1)
//    x.notify(t_succ_change_x, true, None)
//    assert(TestHost.sgt.ensureOrder(t_discover, t_succ_change_x) == FirstFirst)
//
//    val t_succ_nochange_x_frame_y = Transaction().branches(2)
//    x.incrementFrame(t_succ_nochange_x_frame_y)
//    y.incrementFrame(t_succ_nochange_x_frame_y)
//    t_succ_nochange_x_frame_y.start().branches(1)
//    x.notify(t_succ_nochange_x_frame_y, false, None)
//
//    val t_succ_nochange_x = Transaction().branches(1)
//    x.incrementFrame(t_succ_nochange_x)
//    t_succ_nochange_x.start().branches(1)
//    x.notify(t_succ_nochange_x, false, None)
//
//    val t_succ_change_x_queuedchange_y = Transaction().branches(2)
//    x.incrementFrame(t_succ_change_x_queuedchange_y)
//    y.incrementFrame(t_succ_change_x_queuedchange_y)
//    t_succ_change_x_queuedchange_y.start().branches(2)
//    x.notify(t_succ_change_x_queuedchange_y, true, None)
//    y.notify(t_succ_change_x_queuedchange_y, true, None)
//
//    val t_succ_frame_x_nochange_y = Transaction().branches(2)
//    x.incrementFrame(t_succ_frame_x_nochange_y)
//    y.incrementFrame(t_succ_frame_x_nochange_y)
//    t_succ_frame_x_nochange_y.start().branches(1)
//    y.notify(t_succ_frame_x_nochange_y, false, None)
//
//    val t_succ_queuedchange_x = Transaction().branches(1)
//    x.incrementFrame(t_succ_queuedchange_x)
//    t_succ_queuedchange_x.start().branches(1)
//    x.notify(t_succ_queuedchange_x, true, None)
//
//    assertVersions(x,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t_pred_nochange, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_pred_change, Set.empty, pending = 0, changed = 0, Some(t_pred_change)),
//      // discovery will be inserted here
//      new Version(t_succ_change_x, Set.empty, pending = 0, changed = 0, Some(t_succ_change_x)),
//      new Version(t_succ_nochange_x_frame_y, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_succ_nochange_x, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set.empty, pending = 0, changed = 0, Some(t_succ_change_x_queuedchange_y)),
//      new Version(t_succ_frame_x_nochange_y, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_queuedchange_x, Set.empty, pending = 0, changed = 1, None))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t_discover, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_nochange_x_frame_y, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set.empty, pending = 0, changed = 1, None),
//      new Version(t_succ_frame_x_nochange_y, Set.empty, pending = 0, changed = 0, None))
//
//    y.notify(t_discover, true, None)
//
//    assertVersions(x,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t_pred_nochange, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_pred_change, Set.empty, pending = 0, changed = 0, Some(t_pred_change)),
//      new Version(t_discover, Set(y), pending = 0, changed = 0, None),
//      new Version(t_succ_change_x, Set(y), pending = 0, changed = 0, Some(t_succ_change_x)),
//      new Version(t_succ_nochange_x_frame_y, Set(y), pending = 0, changed = 0, None),
//      new Version(t_succ_nochange_x, Set(y), pending = 0, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set(y), pending = 0, changed = 0, Some(t_succ_change_x_queuedchange_y)),
//      new Version(t_succ_frame_x_nochange_y, Set(y), pending = 1, changed = 0, None),
//      new Version(t_succ_queuedchange_x, Set(y), pending = 0, changed = 1, None))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t_discover, Set.empty, pending = 0, changed = 0, Some(t_discover)),
//      new Version(t_succ_change_x, Set.empty, pending = 0, changed = 0, Some(t_succ_change_x)),
//      new Version(t_succ_nochange_x_frame_y, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set.empty, pending = 0, changed = 2, None),
//      new Version(t_succ_frame_x_nochange_y, Set.empty, pending = 1, changed = 0, None))
//  }
//
//  "A dynamic drop" should "redecrement frames as needed" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
//    val t_drop = Transaction().branches(1)
//    val y = new SignalVersionList(TestHost, t, t, {
//      (ticket: ReevaluationTicket, v_in: Transaction) =>
//        if(ticket.txn == t_drop) x.drop(ticket)
//        tracker.comp(ticket, v_in)
//    })
//    x.discoverSuspend(ReevaluationTicket(t, y))
//    t.done()
//
//    val t_pred_nochange = Transaction().branches(1)
//    x.incrementFrame(t_pred_nochange)
//    t_pred_nochange.start().branches(1)
//    x.notify(t_pred_nochange, false, None)
//
//    val t_pred_change = Transaction().branches(1)
//    x.incrementFrame(t_pred_change)
//    t_pred_change.start().branches(1)
//    x.notify(t_pred_change, true, None)
//
//    y.incrementFrame(t_drop)
//    t_drop.start()
//
//    val t_succ_change_x = Transaction().branches(1)
//    x.incrementFrame(t_succ_change_x)
//    t_succ_change_x.start().branches(1)
//    x.notify(t_succ_change_x, true, None)
//
//    val t_succ_nochange_x_frame_y = Transaction().branches(2)
//    x.incrementFrame(t_succ_nochange_x_frame_y)
//    y.incrementFrame(t_succ_nochange_x_frame_y)
//    t_succ_nochange_x_frame_y.start().branches(1)
//
//    val t_succ_nochange_x = Transaction().branches(1)
//    x.incrementFrame(t_succ_nochange_x)
//    t_succ_nochange_x.start().branches(1)
//    x.notify(t_succ_nochange_x, false, None)
//
//    x.notify(t_succ_nochange_x_frame_y, false, None)
//
//    val t_succ_change_x_queuedchange_y = Transaction().branches(2)
//    x.incrementFrame(t_succ_change_x_queuedchange_y)
//    y.incrementFrame(t_succ_change_x_queuedchange_y)
//    t_succ_change_x_queuedchange_y.start().branches(2)
//    x.notify(t_succ_change_x_queuedchange_y, true, None)
//    y.notify(t_succ_change_x_queuedchange_y, true, None)
//
//    val t_succ_frame_x_nochange_y = Transaction().branches(2)
//    x.incrementFrame(t_succ_frame_x_nochange_y)
//    y.incrementFrame(t_succ_frame_x_nochange_y)
//    t_succ_frame_x_nochange_y.start().branches(1)
//    y.notify(t_succ_frame_x_nochange_y, false, None)
//
//    val t_succ_queuedchange_x = Transaction().branches(1)
//    x.incrementFrame(t_succ_queuedchange_x)
//    t_succ_queuedchange_x.start().branches(1)
//    x.notify(t_succ_queuedchange_x, true, None)
//
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t_pred_nochange, Set(y), pending = 0, changed = 0, None),
//      new Version(t_pred_change, Set(y), pending = 0, changed = 0, Some(t_pred_change)),
//      // drop will be inserted here
//      new Version(t_succ_change_x, Set(y), pending = 0, changed = 0, Some(t_succ_change_x)),
//      new Version(t_succ_nochange_x_frame_y, Set(y), pending = 0, changed = 0, None),
//      new Version(t_succ_nochange_x, Set(y), pending = 0, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set(y), pending = 0, changed = 0, Some(t_succ_change_x_queuedchange_y)),
//      new Version(t_succ_frame_x_nochange_y, Set(y), pending = 1, changed = 0, None),
//      new Version(t_succ_queuedchange_x, Set(y), pending = 0, changed = 1, None))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t_pred_nochange, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_pred_change, Set.empty, pending = 0, changed = 0, Some(t_pred_change)),
//      new Version(t_drop, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_change_x, Set.empty, pending = 0, changed = 1, None),
//      new Version(t_succ_nochange_x_frame_y, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set.empty, pending = 0, changed = 2, None),
//      new Version(t_succ_frame_x_nochange_y, Set.empty, pending = 1, changed = 0, None))
//
//    y.notify(t_drop, true, None)
//
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t_pred_nochange, Set(y), pending = 0, changed = 0, None),
//      new Version(t_pred_change, Set(y), pending = 0, changed = 0, Some(t_pred_change)),
//      new Version(t_drop, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_succ_change_x, Set.empty, pending = 0, changed = 0, Some(t_succ_change_x)),
//      new Version(t_succ_nochange_x_frame_y, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_succ_nochange_x, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set.empty, pending = 0, changed = 0, Some(t_succ_change_x_queuedchange_y)),
//      new Version(t_succ_frame_x_nochange_y, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_queuedchange_x, Set.empty, pending = 0, changed = 1, None))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t_pred_nochange, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_pred_change, Set.empty, pending = 0, changed = 0, Some(t_pred_change)),
//      new Version(t_drop, Set.empty, pending = 0, changed = 0, Some(t_drop)),
//      new Version(t_succ_change_x, Set.empty, pending = 0, changed = 0, None),
//      new Version(t_succ_nochange_x_frame_y, Set.empty, pending = 1, changed = 0, None),
//      new Version(t_succ_change_x_queuedchange_y, Set.empty, pending = 0, changed = 1, None),
//      new Version(t_succ_frame_x_nochange_y, Set.empty, pending = 0, changed = 0, None))
//    }
//}
