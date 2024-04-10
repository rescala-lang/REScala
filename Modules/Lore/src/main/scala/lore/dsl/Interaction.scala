package lore.dsl

import lore.dsl.*
import reactives.default.{Var as Source, *}

import scala.annotation.targetName

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

trait CanExecute[S <: Tuple, A] {
  type E[_ <: S, _ <: A] <: Interaction[S, A]

  def executes(fun: (S, A) => S): E[S, A]

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
    def modifies(source: Source[T]):
    InteractionWithModifies[Tuple1[T], Tuple1[Source[T]], A] = {
      InteractionWithModifies(m.requires, m.ensures, Tuple1(source))
    }
  }

  extension [T1, T2, A](m: UnboundInteraction[(T1, T2), A]) {
    def modifies(source1: Source[T1], source2: Source[T2]):
    InteractionWithModifies[(T1, T2), (Source[T1], Source[T2]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2))
  }

  extension [T1, T2, T3, A](m: UnboundInteraction[(T1, T2, T3), A]) {
    def modifies(source1: Source[T1], source2: Source[T2], source3: Source[T3]):
    InteractionWithModifies[(T1, T2, T3), (Source[T1], Source[T2], Source[T3]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2, source3))
  }

  extension [T1, T2, T3, T4, A](m: UnboundInteraction[(T1, T2, T3, T4), A]) {
    def modifies(source1: Source[T1], source2: Source[T2], source3: Source[T3], source4: Source[T4]):
    InteractionWithModifies[(T1, T2, T3, T4), (Source[T1], Source[T2], Source[T3], Source[T4]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2, source3, source4))
  }

  extension [T1, T2, T3, T4, T5, A](m: UnboundInteraction[(T1, T2, T3, T4, T5), A]) {
    def modifies(source1: Source[T1], source2: Source[T2], source3: Source[T3], source4: Source[T4], source5: Source[T5]):
    InteractionWithModifies[(T1, T2, T3, T4, T5), (Source[T1], Source[T2], Source[T3], Source[T4], Source[T5]), A] =
      InteractionWithModifies(m.requires, m.ensures, (source1, source2, source3, source4, source5))
  }

  extension [T, A](m: InteractionWithExecutes[Tuple1[T], A]) {
    def modifies(source: Source[T]):
    BoundInteraction[Tuple1[T], Tuple1[Source[T]], A] = {
      BoundInteraction(m.requires, m.ensures, m.executes, Tuple1(source))
    }
  }

  extension [T1, T2, A](m: InteractionWithExecutes[(T1, T2), A]) {
    def modifies(source1: Source[T1], source2: Source[T2]):
    BoundInteraction[(T1, T2), (Source[T1], Source[T2]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2))
  }

  extension [T1, T2, T3, A](m: InteractionWithExecutes[(T1, T2, T3), A]) {
    def modifies(source1: Source[T1], source2: Source[T2], source3: Source[T3]):
    BoundInteraction[(T1, T2, T3), (Source[T1], Source[T2], Source[T3]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2, source3))
  }

  extension [T1, T2, T3, T4, A](m: InteractionWithExecutes[(T1, T2, T3, T4), A]) {
    def modifies(source1: Source[T1], source2: Source[T2], source3: Source[T3], source4: Source[T4]):
    BoundInteraction[(T1, T2, T3, T4), (Source[T1], Source[T2], Source[T3], Source[T4]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2, source3, source4))
  }

  extension [T1, T2, T3, T4, T5, A](m: InteractionWithExecutes[(T1, T2, T3, T4, T5), A]) {
    def modifies(source1: Source[T1], source2: Source[T2], source3: Source[T3], source4: Source[T4], source5: Source[T5]):
    BoundInteraction[(T1, T2, T3, T4, T5), (Source[T1], Source[T2], Source[T3], Source[T4], Source[T5]), A] =
      BoundInteraction(m.requires, m.ensures, m.executes, (source1, source2, source3, source4, source5))
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
