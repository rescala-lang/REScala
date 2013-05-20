package reswing

import scala.swing.Panel

abstract class RePanel extends ReComponent {
  override protected val peer: Panel with ComponentMixin
}
