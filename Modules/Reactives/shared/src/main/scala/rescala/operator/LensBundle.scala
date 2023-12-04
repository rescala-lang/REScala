//package rescala.operator
//
//trait LensBundle {
//  //selfType: Operators =>
//
//  trait Lens[M, V] {
//    def toView(m: M): V
//
//    def toModel(v: V, m: M): M
//  }
//
//  trait BijectiveLens[M, V] extends Lens[M, V] {
//    def toView(m: M): V
//
//    def toModel(v: V): M
//
//    def toModel(v: V, m: M): M = toModel(v)
//  }
//
//  class AddLens[A](k: A)(implicit num: Numeric[A]) extends BijectiveLens[A, A] {
//    def toView(m: A): A = num.plus(m, k)
//
//    def toModel(v: A): A = num.minus(v, k)
//  }


//  class LVar extends Var()
//
//   //das war ja eher nur einen auf fancy shmancy machen, also fuer uns eher irrelevant (?)
//  def LVar[A](init: A): LVar[A]
//  def applyLens[B](lens: Lens[A, B]): LVar[B]
//
//  class FractionalLVar[A: Fractional](lvar: LVar[A]) {
//    def *(k:A) = lvar.applyLens(new MulLens(k))
//  }
//}
//
//  //like this?
//  class SubtractLens[A](k: A)(implicit num: Numeric[A]) extends BijectiveLens[A, A] {
//    def toView(m: A): A = num.minus(m, k)
//    def toModel(v: A): A = num.plus(v, k)
//  }
//
//  class MulLens[A](k: A)(implicit frac: Fractional[A]) extends Lens[A, A] {
//    if(k==0) throw new IllegalArgumentException("Illegal Lens: mul/div by zero")
//
//    def toView(m: A): A = frac.times(m, k)
//    def toModel(v: A, m: A): A = {
//      val res = frac.div(v, k)
//      if(frac.equiv(res, m)) m
//      else res
//    }
//  }
//
//  //analog auch für andere arithmetische Operations-Lenses
//  //Division, Exponential, Wurzel, Log (?)
//
//  /**
//   //das war ja eher nur einen auf fancy shmancy machen, also fuer uns eher irrelevant (?)
//  def LVar[A](init: A): LVar[A]
//  def applyLens[B](lens: Lens[A, B]): LVar[B]
//
//  implicit def toFracLVar[A: Fractional](lvar: LVar[A]) = new FractionalLVar(lvar)
//
//  class FractionalLVar[A: Fractional](lvar: LVar[A]) {
//    def *(k:A) = lvar.applyLens(new MulLens(k))
//  }
//  **/
//
//
//}
