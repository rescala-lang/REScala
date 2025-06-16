package compiler

import compiler.context.TranslationContext

import scala.annotation.nowarn

trait CompilerFragment {
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline def ensureCtx[T <: TranslationContext]
      : [A, R] => (T ?=> PartialFunction[A, R]) => TranslationContext ?=> PartialFunction[A, R] =
    [A, R] =>
      (a: T ?=> PartialFunction[A, R]) =>
        (ctx: TranslationContext) ?=>
          ctx match {
            case t: T => a(using t)
            case _    => PartialFunction.empty[A, R]
        }
}
