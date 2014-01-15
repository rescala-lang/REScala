package main.abstraction

import react._
import scala.language.higherKinds

trait SignalWrappable[WrappedType, WrapperType] {
    def wrap(unwrapped: Signal[WrappedType]): WrapperType
}