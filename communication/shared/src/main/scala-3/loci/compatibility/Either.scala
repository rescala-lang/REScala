package loci
package compatibility

object either:
  private[loci] inline def left[L, R](either: Either[L, R]) =
    either.left.toOption.get

  private[loci] inline def right[L, R](either: Either[L, R]) =
    either.toOption.get

  private[loci] inline def map[L, R, RO](either: Either[L, R])(f: R => RO) =
    either map f

  private[loci] inline def flatMap[L, R, LO >: L, RO](either: Either[L, R])(f: R => Either[LO, RO]) =
    either flatMap f

  private[loci] inline def foreach[L, R, T](either: Either[L, R])(f: R => T) =
    either foreach f
