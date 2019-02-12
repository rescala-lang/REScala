package ersirjs.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("cytoscape", JSImport.Namespace)
@js.native
object Cytoscape extends js.Object {
  def apply(container: js.Object): js.Dynamic = js.native
}
