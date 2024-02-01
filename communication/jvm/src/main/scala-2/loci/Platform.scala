package loci

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.util.Properties

@compileTimeOnly("enable macro paradise to use platform-specific code")
final class platform(cond: Boolean) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro utility.platform.annotation
}

object platform {
  final val jvm = true
  final val js = false

  final val mac = Properties.isMac
  final val win = Properties.isWin

  def apply[T](cond: Boolean)(body: T): Unit = macro utility.platform.apply[T]

  def value[T](selection: Selection[T]*): T = macro utility.platform.value[T]

  sealed trait Selection[T]

  sealed trait UnconditionalSelection {
    @compileTimeOnly("illegal use of platform-specific code")
    implicit def unconditional[T](value: T): Selection[T] =
      throw new NotImplementedError("illegal use of platform-specific code")
  }

  object Selection extends UnconditionalSelection {
    @compileTimeOnly("illegal use of platform-specific code")
    implicit def conditional[T](value: (Boolean, T)): Selection[T] =
      throw new NotImplementedError("illegal use of platform-specific code")
  }
}
