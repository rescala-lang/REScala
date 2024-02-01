package loci

import scala.util.Properties

object platform:
  final val jvm = false
  final val js = true

  final val mac = Properties.isMac
  final val win = Properties.isWin

  inline def apply[T](inline cond: Boolean)(inline body: T): Unit =
    ${ utility.platform.apply('cond)('body) }

  inline def value[T](inline selection: ((Boolean, T) | T)*): T =
    ${ utility.platform.value('selection) }
end platform
