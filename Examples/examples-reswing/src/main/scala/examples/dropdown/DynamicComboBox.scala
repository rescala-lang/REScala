package examples.dropdown

import reswing._
import scala.swing.ComboBox
import javax.swing.JComboBox
import scala.swing.event.KeyPressed
import scala.swing.event.FocusGained
import scala.swing.event.SelectionChanged
import javax.swing.ComboBoxModel
import java.awt.Color
import reswing.ReSwingEvent
import scala.swing.event.SelectionChanged





class DynamicComboBox[A] extends ComboBox[A](Nil: List[A]) {
  val peerBox: JComboBox[_] = this.peer.asInstanceOf[JComboBox[_]]

  /** Set the choices */
  def setChoices(options: List[A]) {
    val currentIdx = selection.index
    val model = ComboBox.newConstantModel(options).asInstanceOf[ComboBoxModel[String]]
    peerBox.asInstanceOf[JComboBox[String]].setModel(model)
    if(currentIdx < options.length)
    	selection.index = currentIdx
  }
  
  def getChoices: List[A] = Nil
}



class ReDynamicComboBox[A]( 
    val options: ReSwingValue[List[A]] = ReSwingNoValue[List[A]],
    val selection: ReSwingValue[Int] = (),
    val selectionForeground: ReSwingValue[Color] = (),
    val selectionBackground: ReSwingValue[Color] = (),
    val selectIndices: ReSwingEvent[Seq[Int]] = ()) extends ReComponent {
  
  override protected lazy val peer = new DynamicComboBox[A] with ComponentMixin
  
  (options using (peer.getChoices _, peer.setChoices _, classOf[Nothing]))
  (selection using (peer.selection.index _, peer.selection.index_= _, (peer.selection, classOf[SelectionChanged])))
}