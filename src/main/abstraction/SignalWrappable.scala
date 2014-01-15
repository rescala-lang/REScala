package main.abstraction

import react._
import scala.language.higherKinds

trait SignalWrappable0[WrappedType, WrapperType] {
    def wrap(unwrapped: Signal[WrappedType]): WrapperType
}

trait SignalWrappable1[WrappedType[_], WrapperType[_]] {
    def wrap[A](unwrapped: Signal[WrappedType[A]]): WrapperType[A]
}

trait SignalWrappable2[WrappedType[_,_], WrapperType[_,_]] {
    def wrap[A,B](unwrapped: Signal[WrappedType[A,B]]): WrapperType[A,B]
}

trait SignalWrappable3[WrappedType[_,_,_], WrapperType[_,_,_]] {
    def wrap[A,B,C](unwrapped: Signal[WrappedType[A,B,C]]): WrapperType[A,B,C]
}