package ex201x.reswingexamples.dropdown

import ex2013reswing.{ReComponent, ReSwingEvent, ReSwingNoValue, ReSwingValue}

import java.awt.Color
import javax.swing.{ComboBoxModel, JComboBox}
import scala.swing.ComboBox
import scala.swing.event.SelectionChanged

class DynamicComboBox[A] extends ComboBox[A](Nil: List[A]) {
  val peerBox: JComboBox[?] = this.peer.asInstanceOf[JComboBox[?]]

  /** Set the choices */
  def setChoices(options: List[A]): Unit = {
    val currentIdx = selection.index
    val model      = ComboBox.newConstantModel(options).asInstanceOf[ComboBoxModel[String]]
    peerBox.asInstanceOf[JComboBox[String]].setModel(model)
    if currentIdx < options.length then
      selection.index = currentIdx
  }

  def getChoices: List[A] = Nil
}

class ReDynamicComboBox[A](
    val options: ReSwingValue[List[A]] = ReSwingNoValue[List[A]](),
    val selection: ReSwingValue[Int] = (),
    val selectionForeground: ReSwingValue[Color] = (),
    val selectionBackground: ReSwingValue[Color] = (),
    val selectIndices: ReSwingEvent[Seq[Int]] = ()
) extends ReComponent {

  override protected lazy val peer: DynamicComboBox[A] & ComponentMixin = new DynamicComboBox[A] with ComponentMixin

  options.using(() => peer.getChoices, peer.setChoices, classOf[Nothing])
  selection.using(() => peer.selection.index, peer.selection.index_=, (peer.selection, classOf[SelectionChanged]))
}
