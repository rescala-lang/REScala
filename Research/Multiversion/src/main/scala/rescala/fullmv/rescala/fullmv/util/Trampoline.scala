package rescala.fullmv.rescala.fullmv.util

import scala.annotation.tailrec

/**
  * Tool to enable tail call optimization for mutually recursive methods.
  * @tparam A the recursion's result type
  */
sealed trait Trampoline[+A] {
  /**
    * Executes all recursion steps until termination; guaranteed no stack-overflow due to tail call optimization.
    * @return the final result.
    */
  @tailrec final
  def bounce: A = this match {
    case Result(a) => a
    case Recurse(step) => step().bounce
  }
}

/**
  * Wrapper for a recursion result.
  * @param result the result value
  * @tparam A the recursion's result type
  */
final case class Result[A](result: A) extends Trampoline[A]

/**
  * Wrapper for a next recursion step.
  * @param step the step
  * @tparam A the recursion's result type
  */
final case class Recurse[A](step: () => Trampoline[A]) extends Trampoline[A]

