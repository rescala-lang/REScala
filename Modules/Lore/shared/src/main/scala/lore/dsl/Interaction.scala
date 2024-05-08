package lore.dsl

import lore.dsl.*
import reactives.core.ReSource

import scala.annotation.{static, targetName}
import scala.quoted.*
import reactives.default.*
import reactives.operator.Interface.State as BundleState

trait Interaction[S <: Tuple, A] {
  type T[_ <: S, _ <: A] <: Interaction[S, A]

  inline def requires(inline pred: (S, A) => Boolean): T[S, A]

  inline def requires[B](inline pred: (B, A) => Boolean)(using S =:= Tuple1[B]): T[S, A] =
    requires({ (s, a) => pred(s._1, a) })

  inline def ensures(inline pred: (S, A) => Boolean): T[S, A]

  inline def ensures[B](inline pred: (B, A) => Boolean)(using S =:= Tuple1[B]): T[S, A] =
    ensures({ (s, a) => pred(s._1, a) })
}

object Interaction {
  @targetName("apply1")
  inline def apply[S, A]: UnboundInteraction[Tuple1[S], A] =
    UnboundInteraction(Seq.empty, Seq.empty)

  @targetName("apply2")
  inline def apply[S1, S2, A]: UnboundInteraction[(S1, S2), A] =
    UnboundInteraction(Seq.empty, Seq.empty)

  @targetName("apply3")
  inline def apply[S1, S2, S3, A]: UnboundInteraction[(S1, S2, S3), A] =
    UnboundInteraction(Seq.empty, Seq.empty)

  @targetName("apply4")
  inline def apply[S1, S2, S3, S4, A]: UnboundInteraction[(S1, S2, S3, S4), A] =
    UnboundInteraction(Seq.empty, Seq.empty)

  @targetName("apply5")
  inline def apply[S1, S2, S3, S4, S5, A]: UnboundInteraction[(S1, S2, S3, S4, S5), A] =
    UnboundInteraction(Seq.empty, Seq.empty)

}

trait CanAct[S <: Tuple, A] {
  type AO[_ <: S, _ <: A] <: Interaction[S, A]

  def actsOn(event: Event[A]): AO[S, A]
}

trait CanExecute[S <: Tuple, A] {
  type E[_ <: S, _ <: A] <: Interaction[S, A]

  def executes(fun: (S, A) => S): E[S, A]

  inline def executes[B](inline fun: (B, A) => B)(using ev: S =:= Tuple1[B]): E[S, A] =
    executes({ (s, a) => ev.flip(Tuple1(fun(s._1, a))) })

}

trait Executes[S <: Tuple, A] {
  val executes: (S, A) => S
}

implicit object Ex {

  extension [S, A](e: CanExecute[Tuple1[S], A]) {
    def executes(fun: (S, A) => S): e.E[Tuple1[S], A] =
      e.executes((s, a) => Tuple1(fun(s._1, a)))
  }

  extension [S1, S2, A](e: CanExecute[(S1, S2), A]) {
    def executes(fun: (S1, S2, A) => (S1, S2)): e.E[(S1, S2), A] =
      e.executes((s, a) => fun(s._1, s._2, a))
  }

  extension [S1, S2, S3, A](e: CanExecute[(S1, S2, S3), A]) {
    def executes(fun: (S1, S2, S3, A) => (S1, S2, S3)): e.E[(S1, S2, S3), A] =
      e.executes((s, a) => fun(s._1, s._2, s._3, a))
  }

  extension [S1, S2, S3, S4, A](e: CanExecute[(S1, S2, S3, S4), A]) {
    def executes(fun: (S1, S2, S3, S4, A) => (S1, S2, S3, S4)): e.E[(S1, S2, S3, S4), A] =
      e.executes((s, a) => fun(s._1, s._2, s._3, s._4, a))
  }

  extension [S1, S2, S3, S4, S5, A](e: CanExecute[(S1, S2, S3, S4, S5), A]) {
    def executes(fun: (S1, S2, S3, S4, S5, A) => (S1, S2, S3, S4, S5)): e.E[(S1, S2, S3, S4, S5), A] =
      e.executes((s, a) => fun(s._1, s._2, s._3, s._4, s._5, a))
  }

  extension [T, A](m: UnboundInteraction[Tuple1[T], A]) {
    def modifies(source: Var[T]): InteractionWithModifies[Tuple1[T], Tuple1[Var[T]], A] = {
      InteractionWithModifies(m.requires, m.ensures, Tuple1(source))
    }
  }

  extension [T1, T2, A](m: UnboundInteraction[(T1, T2), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2]
    ): InteractionWithModifies[(T1, T2), (Var[T1], Var[T2]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2))
  }

  extension [T1, T2, T3, A](m: UnboundInteraction[(T1, T2, T3), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3]
    ): InteractionWithModifies[(T1, T2, T3), (Var[T1], Var[T2], Var[T3]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2, source3))
  }

  extension [T1, T2, T3, T4, A](m: UnboundInteraction[(T1, T2, T3, T4), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4]
    ): InteractionWithModifies[(T1, T2, T3, T4), (Var[T1], Var[T2], Var[T3], Var[T4]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2, source3, source4))
  }

  extension [T1, T2, T3, T4, T5, A](m: UnboundInteraction[(T1, T2, T3, T4, T5), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4],
        source5: Var[T5]
    ): InteractionWithModifies[(T1, T2, T3, T4, T5), (Var[T1], Var[T2], Var[T3], Var[T4], Var[T5]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2, source3, source4, source5))
  }

  extension [T, A](m: InteractionWithExecutes[Tuple1[T], A]) {
    def modifies(source: Var[T]): InteractionWithExecutesAndModifies[Tuple1[T], Tuple1[Var[T]], A] = {
      InteractionWithExecutesAndModifies(m.requires, m.ensures, m.executes, Tuple1(source))
    }
  }

  extension [T1, T2, A](m: InteractionWithExecutes[(T1, T2), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2]
    ): InteractionWithExecutesAndModifies[(T1, T2), (Var[T1], Var[T2]), A] =
      InteractionWithExecutesAndModifies(m.requires, m.ensures, m.executes, (source1, source2))
  }

  extension [T1, T2, T3, A](m: InteractionWithExecutes[(T1, T2, T3), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3]
    ): InteractionWithExecutesAndModifies[(T1, T2, T3), (Var[T1], Var[T2], Var[T3]), A] =
      InteractionWithExecutesAndModifies(m.requires, m.ensures, m.executes, (source1, source2, source3))
  }

  extension [T1, T2, T3, T4, A](m: InteractionWithExecutes[(T1, T2, T3, T4), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4]
    ): InteractionWithExecutesAndModifies[(T1, T2, T3, T4), (Var[T1], Var[T2], Var[T3], Var[T4]), A] =
      InteractionWithExecutesAndModifies(m.requires, m.ensures, m.executes, (source1, source2, source3, source4))
  }

  extension [T1, T2, T3, T4, T5, A](m: InteractionWithExecutes[(T1, T2, T3, T4, T5), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4],
        source5: Var[T5]
    ): InteractionWithExecutesAndModifies[(T1, T2, T3, T4, T5), (Var[T1], Var[T2], Var[T3], Var[T4], Var[T5]), A] =
      InteractionWithExecutesAndModifies(
        m.requires,
        m.ensures,
        m.executes,
        (source1, source2, source3, source4, source5)
      )
  }

  extension [T, A](m: InteractionWithActs[Tuple1[T], A]) {
    def modifies(source: Var[T]): InteractionWithModifiesAndActs[Tuple1[T], Tuple1[Var[T]], A] = {
      InteractionWithModifiesAndActs(m.requires, m.ensures, Tuple1(source), m.event)
    }
  }

  extension [T1, T2, A](m: InteractionWithActs[(T1, T2), A]) {
    def modifies(source1: Var[T1], source2: Var[T2]): InteractionWithModifiesAndActs[(T1, T2), (Var[T1], Var[T2]), A] =
      InteractionWithModifiesAndActs(m.requires, m.ensures, (source1, source2), m.event)
  }

  extension [T1, T2, T3, A](m: InteractionWithActs[(T1, T2, T3), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3]
    ): InteractionWithModifiesAndActs[(T1, T2, T3), (Var[T1], Var[T2], Var[T3]), A] =
      InteractionWithModifiesAndActs(m.requires, m.ensures, (source1, source2, source3), m.event)
  }

  extension [T1, T2, T3, T4, A](m: InteractionWithActs[(T1, T2, T3, T4), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4]
    ): InteractionWithModifiesAndActs[(T1, T2, T3, T4), (Var[T1], Var[T2], Var[T3], Var[T4]), A] =
      InteractionWithModifiesAndActs(m.requires, m.ensures, (source1, source2, source3, source4), m.event)
  }

  extension [T1, T2, T3, T4, T5, A](m: InteractionWithActs[(T1, T2, T3, T4, T5), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4],
        source5: Var[T5]
    ): InteractionWithModifiesAndActs[(T1, T2, T3, T4, T5), (Var[T1], Var[T2], Var[T3], Var[T4], Var[T5]), A] =
      InteractionWithModifiesAndActs(m.requires, m.ensures, (source1, source2, source3, source4, source5), m.event)
  }

  extension [T, A](m: InteractionWithExecutesAndActs[Tuple1[T], A]) {
    def modifies(source: Var[T]): BoundInteraction[Tuple1[T], Tuple1[Var[T]], A] = {
      BoundInteraction(m.requires, m.ensures, m.executes, Tuple1(source), m.event)
    }
  }

  extension [T1, T2, A](m: InteractionWithExecutesAndActs[(T1, T2), A]) {
    def modifies(source1: Var[T1], source2: Var[T2]): BoundInteraction[(T1, T2), (Var[T1], Var[T2]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2), m.event)
  }

  extension [T1, T2, T3, A](m: InteractionWithExecutesAndActs[(T1, T2, T3), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3]
    ): BoundInteraction[(T1, T2, T3), (Var[T1], Var[T2], Var[T3]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2, source3), m.event)
  }

  extension [T1, T2, T3, T4, A](m: InteractionWithExecutesAndActs[(T1, T2, T3, T4), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4]
    ): BoundInteraction[(T1, T2, T3, T4), (Var[T1], Var[T2], Var[T3], Var[T4]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2, source3, source4), m.event)
  }

  extension [T1, T2, T3, T4, T5, A](m: InteractionWithExecutesAndActs[(T1, T2, T3, T4, T5), A]) {
    def modifies(
        source1: Var[T1],
        source2: Var[T2],
        source3: Var[T3],
        source4: Var[T4],
        source5: Var[T5]
    ): BoundInteraction[(T1, T2, T3, T4, T5), (Var[T1], Var[T2], Var[T3], Var[T4], Var[T5]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2, source3, source4, source5), m.event)
  }

  extension (left: Boolean) {
    infix def implies(right: Boolean): Boolean = !left || right

    @targetName("impliesOp")
    def ==>(right: Boolean): Boolean = !left || right

    infix def equiv(right: Boolean): Boolean = left == right

    @targetName("equivOP")
    def <==>(right: Boolean): Boolean = left == right
  }
  inline def assume(inline expr: Boolean): Unit = {}

}
