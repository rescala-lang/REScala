package ex2024travel.lofi_acl.example.monotonic_acl

import lofi_acl.access.PermissionTree
import lofi_acl.crypto.PublicIdentity
import ex2024travel.lofi_acl.example.travelplanner.TravelPlan
import lofi_acl.sync.acl.monotonic.MonotonicAcl
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.{Clipboard, ClipboardContent}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.util.StringConverter

import scala.concurrent.ExecutionContext

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

  private val createInviteButton = Button("Create Invite")
  createInviteButton.onAction = _ => {
    val clipboard = Clipboard.systemClipboard
    val content   = new ClipboardContent()
    content.putString(inviteText.getText)
    val _ = clipboard.setContent(content)
    rootPane.bottom = inviteText
    permissionTreePane.disable = true
    Platform.runLater { () =>
      try {
        val perms = permissionTreePane.selectionToPermissions
        ExecutionContext.global.execute { () =>
          travelPlanModel.grantPermission(
            PublicIdentity.fromPublicKey(invitation.identityKey.getPublic),
            perms._1,
            perms._2
          )
        }
      } catch
        case e: Exception =>
          e.printStackTrace()
          Alert(Alert.AlertType.Error, e.toString).show()
    }
  }

  private val delegatePermissionsButton = Button("Delegate permissions")
  delegatePermissionsButton.onAction = _ => {
    val permissionReceiverComboBox = {
      val otherReplicas = travelPlanModel.currentAcl.read.keySet.filterNot(_ == travelPlanModel.publicId).toSeq
      val comboBox      = ComboBox(otherReplicas)
      comboBox.converter = StringConverter[PublicIdentity](
        fromStringFunction = str => if str == null || str.isEmpty then null else PublicIdentity(str),
        toStringFunction = pubId => if pubId == null then "" else pubId.id
      )
      comboBox
    }
    val confirmationButton = Button("Delegate")
    confirmationButton.disable <== permissionReceiverComboBox.selectionModel.value.selectedItemProperty().isNull
    rootPane.bottom = HBox(permissionReceiverComboBox, confirmationButton)

    confirmationButton.onAction = _ => {
      val receivingReplica = permissionReceiverComboBox.getValue
      Platform.runLater { () =>
        try {
          val perms = permissionTreePane.selectionToPermissions
          ExecutionContext.global.execute { () =>
            travelPlanModel.grantPermission(receivingReplica, perms._1, perms._2)
          }
          permissionReceiverComboBox.value = null
        } catch
          case e: Exception =>
            e.printStackTrace()
            Alert(Alert.AlertType.Error, e.toString).show()
      }
    }
  }

  rootPane.center = permissionTreePane
  rootPane.bottom = HBox(createInviteButton, delegatePermissionsButton)
  content = rootPane
}
