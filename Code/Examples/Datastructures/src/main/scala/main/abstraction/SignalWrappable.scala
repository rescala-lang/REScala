package main.abstraction

import rescala.default._

trait SignalWrappable[WrappedType, WrapperType] {
    def wrap(unwrapped: Signal[WrappedType]): WrapperType
}
