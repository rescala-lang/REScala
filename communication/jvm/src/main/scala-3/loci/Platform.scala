package loci

import scala.util.Properties

object platform:
  final val jvm = true
  final val js = false

  final val mac = Properties.isMac
  final val win = Properties.isWin

  inline def apply[T](inline cond: Boolean)(inline body: T): Unit =
    ${ utility.platform.apply('cond)('body) }

  inline def value[T](inline selection: ((Boolean, T) | T)*): T =
    ${ utility.platform.value('selection) }
end platform
