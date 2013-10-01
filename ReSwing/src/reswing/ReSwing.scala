import scala.swing.Component

package object reswing {
  type CompItem = Component
  object CompItem {
    def apply(elem: Component) = elem
  }
  
  type CompList = Seq[CompItem]
  object CompList {
    def apply(elems: CompItem*) = Seq(elems: _*)
  }
}
