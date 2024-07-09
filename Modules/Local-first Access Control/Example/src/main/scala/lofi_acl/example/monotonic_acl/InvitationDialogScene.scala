package lofi_acl.example.monotonic_acl

import lofi_acl.example.travelplanner.{Expense, TravelPlan}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.Dotted
import rdts.time.CausalTime
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.{Clipboard, ClipboardContent}
import scalafx.scene.layout.BorderPane

class InvitationDialogScene(val invitation: Invitation, val travelPlanModel: TravelPlanModel) extends Scene {
  private val rootPane = BorderPane()

  private given fakeLocalUid: LocalUid = LocalUid(Uid("Test"))
  private var fakeRdt                  = TravelPlan.empty
  fakeRdt = fakeRdt.merge(
    TravelPlan.empty.copy(bucketList =
      fakeRdt.bucketList.mod(_.update("1", LastWriterWins(CausalTime.now(), "A")).toDotted)
    )
  )
  fakeRdt = fakeRdt.merge(
    TravelPlan.empty.copy(bucketList =
      fakeRdt.bucketList.mod(_.update("2", LastWriterWins(CausalTime.now(), "B")).toDotted)
    )
  )
  fakeRdt = fakeRdt.merge(
    TravelPlan.empty.copy(expenses =
      fakeRdt.expenses.mod(_.update(
        "3",
        Expense(LastWriterWins.now(Some("Hello World!")), LastWriterWins.empty, ObserveRemoveMap.empty)
      ).toDotted)
    )
  )

  private val inviteText = TextField()
  inviteText.setText(invitation.encode)
  inviteText.editable = false

  private val createInviteButton = Button("Show Invite")
  createInviteButton.onAction = _ => {
    val clipboard = Clipboard.systemClipboard
    val content   = new ClipboardContent()
    val _         = clipboard.setContent(content)
    rootPane.bottom = inviteText
  }

  rootPane.center = PermissionTreePane(fakeRdt)
  rootPane.bottom = createInviteButton
  content = rootPane
}
