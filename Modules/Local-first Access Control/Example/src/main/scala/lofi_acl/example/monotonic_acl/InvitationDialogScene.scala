package lofi_acl.example.monotonic_acl

import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextArea, TextField}
import scalafx.scene.input.{Clipboard, ClipboardContent}
import scalafx.scene.layout.{Border, BorderPane, HBox}
import scalafx.scene.text.Text

class InvitationDialogScene(val invitation: Invitation, val travelPlanModel: TravelPlanModel) extends Scene {
  private val rootPane = BorderPane()
  private val inviteText = TextField()
  inviteText.setText(invitation.encode)
  inviteText.editable = false
  inviteText.border = Border.Empty

  private val createInviteButton = Button("Show Invite")
  createInviteButton.onAction = _ => {
    val clipboard = Clipboard.systemClipboard
    val content   = new ClipboardContent()
    val _ = clipboard.setContent(content)
    rootPane.bottom = inviteText
  }

  rootPane.bottom = createInviteButton
  content = rootPane
}
