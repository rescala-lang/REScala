package lofi_acl.example.monotonic_acl

import lofi_acl.access.PermissionTree
import lofi_acl.example.travelplanner.TravelPlan
import lofi_acl.sync.acl.monotonic.MonotonicAcl
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.{Clipboard, ClipboardContent}
import scalafx.scene.layout.BorderPane

class InvitationDialogScene(invitation: Invitation, travelPlanModel: TravelPlanModel) extends Scene {
  private val rootPane = BorderPane()

  private val state = travelPlanModel.state

  private val acl: MonotonicAcl[TravelPlan] = travelPlanModel.currentAcl
  private val permissionTreePane = PermissionTreePane(
    state,
    acl.read.getOrElse(invitation.inviter, PermissionTree.empty),
    acl.write.getOrElse(invitation.inviter, PermissionTree.empty),
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
    permissionTreePane.disable = true
  }

  rootPane.center = permissionTreePane
  rootPane.bottom = createInviteButton
  content = rootPane
}
