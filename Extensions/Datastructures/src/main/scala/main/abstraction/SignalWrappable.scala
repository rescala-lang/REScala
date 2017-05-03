package main.abstraction

import rescala._

trait SignalWrappable[WrappedType, WrapperType] {
    def wrap(unwrapped: Signal[WrappedType]): WrapperType
}
